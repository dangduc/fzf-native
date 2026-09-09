/* SPDX-License-Identifier: GPL-3.0-or-later */
/* Exercise the real driver without requiring an external fzf executable. */
#define main fzf_bench_driver_main
#include "fzf-bench-driver.c"
#undef main

static unsigned failures;
#define CHECK(condition) do { \
  if (!(condition)) { \
    fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition); \
    failures++; \
  } \
} while (0)

static uint64_t expected_checksum(size_t items, const uint32_t *indices,
                                  const uint16_t *ranks, size_t count) {
  static const char version[] = "fzf-native-bench-result-v1";
  uint64_t hash = bench_hash_bytes(UINT64_C(14695981039346656037),
                                   version, sizeof version - 1);
  hash = bench_hash_u64(hash, items);
  hash = bench_hash_u64(hash, count);
  for (size_t i = 0; i < count; i++) {
    hash = bench_hash_u64(hash, indices[i]);
    hash = bench_hash_u64(hash, ranks[i]);
  }
  return hash;
}

static void test_fallback_and_duplicate_identity(void) {
  /* Fixture: the 60000-byte gap forces v2 to use v1 with the default slab.
     Pinned fzf 63e82a9 gives raw scores -59914 and 32 for query "ab c".
     The public native membership scores are 37 and 32, respectively.
     Duplicate text deliberately has distinct input identities. */
  char *long_text = malloc(60005);
  if (!long_text) { CHECK(long_text != NULL); return; }
  long_text[0] = 'a';
  memset(long_text + 1, 'x', 60000);
  memcpy(long_text + 60001, "b c", 4);
  char short_text[85];
  memcpy(short_text, "za", 2);
  memset(short_text + 2, 'x', 60);
  short_text[62] = 'b';
  memset(short_text + 63, 'x', 20);
  memcpy(short_text + 83, "c", 2);
  BenchCandidate candidates[] = {
      {.text = long_text, .length = 60004, .index = 0,
       .input_is_ascii = true},
      {.text = short_text, .length = 84, .index = 1,
       .input_is_ascii = true},
      {.text = short_text, .length = 84, .index = 2,
       .input_is_ascii = true},
      {.text = long_text, .length = 60004, .index = 3,
       .input_is_ascii = true},
      {.text = "no match", .length = 8, .index = 4,
       .input_is_ascii = true},
  };
  BenchCorpus corpus = {.candidates = candidates, .count = 5};
  char query[] = "ab c";
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseSmart, false, query, true);
  if (!pattern) { CHECK(pattern != NULL); free(long_text); return; }
  fzf_slab_t *slab = fzf_make_default_slab();
  if (!slab) {
    CHECK(slab != NULL); fzf_free_pattern(pattern); free(long_text); return;
  }
  for (size_t i = 0; i < 2; i++) {
    fzf_score_bounds_t bounds;
    int32_t membership = fzf_get_score_with_bounds_bytes_preclassified(
        candidates[i].text, candidates[i].length, true, pattern, slab, &bounds);
    CHECK(membership == (i == 0 ? 37 : 32));
    CHECK(bounds.raw_score == (i == 0 ? -59914 : 32));
    CHECK(bounds.valid && !fzf_allocation_failed());
  }
  fzf_free_slab(slab);

  const uint32_t sorted_ids[] = {1, 2, 0, 3};
  const uint16_t sorted_ranks[] = {32, 32, 0, 0};
  const uint32_t unsorted_ids[] = {0, 1, 2, 3};
  const uint16_t unsorted_ranks[] = {0, 32, 32, 0};
  for (size_t threads = 1; threads <= 3; threads += 2) {
    for (unsigned sorted = 0; sorted < 2; sorted++) {
      BenchPool pool;
      bool initialized = bench_pool_init(
          &pool, threads, &corpus, pattern, sorted, false);
      CHECK(initialized);
      if (!initialized) continue;
      uint64_t expected = expected_checksum(5,
          sorted ? sorted_ids : unsorted_ids,
          sorted ? sorted_ranks : unsorted_ranks, 4);
      for (unsigned round = 0; round < 12; round++) {
        bench_pool_discard_results(&pool);
        bool ran = bench_pool_run(&pool);
        CHECK(ran);
        if (!ran) break;
        CHECK(bench_pool_result_length(&pool) == 4);
        uint64_t actual = 0;
        CHECK(bench_result_checksum(&pool, 4, &actual));
        CHECK(actual == expected);
        unsigned seen[5] = {0};
        for (size_t w = 0; w < pool.worker_count; w++) {
          for (size_t j = 0; j < pool.workers[w].matches.count; j++) {
            const BenchMatch *match = &pool.workers[w].matches.values[j];
            uint32_t id = match->candidate->index;
            CHECK(id < 4);
            if (id >= 4) continue;
            seen[id]++;
            CHECK(match->membership == ((id == 0 || id == 3) ? 37 : 32));
            CHECK(match->rank_score == ((id == 0 || id == 3) ? 0 : 32));
          }
        }
        for (size_t i = 0; i < 4; i++) CHECK(seen[i] == 1);
      }
      bench_pool_destroy(&pool);
    }
  }
  fzf_free_pattern(pattern);
  free(long_text);
}

static void test_radix_passes(void) {
  enum { count = 512 };
  BenchCandidate candidates[count];
  BenchMatch values[count], expected[count], scratch[count];
  BenchWorker worker = {
      .matches = {values, count, count},
      .sort_scratch = scratch,
      .sort_scratch_capacity = count,
  };
  for (unsigned variant = 0; variant < 5; variant++) {
    memset(scratch, 0xa5, sizeof scratch);
    for (size_t i = 0; i < count; i++) {
      candidates[i] = (BenchCandidate){
          .text = "duplicate", .length = 9, .index = (uint32_t)i,
          .input_is_ascii = true};
      uint16_t rank = variant == 0 ? 32 :
          variant == 1 ? (uint16_t)(i % 7) :
          variant == 2 ? (uint16_t)((i % 7) * 256 + 7) :
          variant == 3 ? (uint16_t)(i * 73) : UINT16_MAX;
      values[i] = (BenchMatch){.candidate = &candidates[i],
                               .membership = 1, .rank_score = rank};
      expected[i] = values[i];
    }
    qsort(expected, count, sizeof *expected, bench_compare_matches);
    bench_sort_matches(&worker);
    for (size_t i = 0; i < count; i++) {
      CHECK(values[i].candidate->index == expected[i].candidate->index);
      CHECK(values[i].rank_score == expected[i].rank_score);
    }
    /* Uniform nonzero bytes must skip scattering, not only zero key bytes. */
    if (variant == 0 || variant == 4) {
      const unsigned char *bytes = (const unsigned char *)scratch;
      for (size_t i = 0; i < sizeof scratch; i++) CHECK(bytes[i] == 0xa5);
    }
  }
}

int main(void) {
  CHECK(sizeof(void *) != 8 || sizeof(BenchMatch) == 16);
  CHECK(bench_rank_score(INT64_MIN) == 0);
  CHECK(bench_rank_score(-1) == 0);
  CHECK(bench_rank_score(0) == 0);
  CHECK(bench_rank_score(32) == 32);
  CHECK(bench_rank_score(UINT16_MAX) == UINT16_MAX);
  CHECK(bench_rank_score((int64_t)UINT16_MAX + 1) == UINT16_MAX);
  CHECK(bench_rank_score(INT64_MAX) == UINT16_MAX);
  test_fallback_and_duplicate_identity();
  test_radix_passes();
  if (failures) return 1;
  puts("benchmark driver raw-rank, duplicate-identity, and radix tests passed");
  return 0;
}
