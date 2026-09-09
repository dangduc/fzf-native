/* SPDX-License-Identifier: GPL-3.0-or-later
 * Reproducible benchmark for persistent-session producer growth.
 *
 * This executable includes the module implementation so it can drive the
 * same AsyncSession, append notification, coordinator, worker-pool, result
 * cache, and publication paths as the Emacs API without measuring Lisp
 * polling overhead.  Timed snapshots are retained and validated against
 * untimed full scans only after every timed growth round has completed.
 */

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../fzf-native-module.c"

typedef struct {
  ScoredStr *top;
  size_t top_count;
  size_t matched_count;
  size_t pool_generation;
  uint64_t checksum;
} BenchSnapshot;

typedef struct {
  bool available;
  size_t pool_generation;
  size_t matched_count;
  size_t depth;
  size_t storage_bytes;
  size_t suffix_compactions_since_flatten;
} BenchMembershipStats;

static double bench_now_ms(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return 0.0;
  return (double)now.tv_sec * 1000.0 + (double)now.tv_nsec / 1e6;
}

static void bench_pause(void) {
  struct timespec pause = {.tv_sec = 0, .tv_nsec = 50000};
  while (nanosleep(&pause, &pause) != 0 && errno == EINTR) {}
}

static uint64_t bench_hash_bytes(uint64_t hash, const void *data,
                                 size_t length) {
  const unsigned char *bytes = data;
  for (size_t i = 0; i < length; i++) {
    hash ^= bytes[i];
    hash *= UINT64_C(1099511628211);
  }
  return hash;
}

static uint64_t bench_snapshot_checksum(const BenchSnapshot *snapshot) {
  uint64_t hash = UINT64_C(14695981039346656037);
  hash = bench_hash_bytes(
      hash, &snapshot->pool_generation, sizeof snapshot->pool_generation);
  hash = bench_hash_bytes(
      hash, &snapshot->matched_count, sizeof snapshot->matched_count);
  hash = bench_hash_bytes(
      hash, &snapshot->top_count, sizeof snapshot->top_count);
  for (size_t i = 0; i < snapshot->top_count; i++) {
    const ScoredStr *value = &snapshot->top[i];
    hash = bench_hash_bytes(hash, &value->idx, sizeof value->idx);
    hash = bench_hash_bytes(hash, &value->score, sizeof value->score);
    hash = bench_hash_bytes(hash, value->str, strlen(value->str) + 1);
  }
  return hash;
}

static AsyncSession *bench_session_create(unsigned workers) {
  AsyncSession *session = calloc(1, sizeof *session);
  if (!session) return NULL;

  atomic_init(&session->lifetime_refs, 1);
  atomic_init(&session->handle_owner_released, false);
  atomic_init(&session->child_owner, AsyncChildUnclaimed);
  atomic_init(&session->producer_state, AsyncProducerRunning);
  atomic_init(&session->producer_error, 0);
  atomic_init(&session->producer_exit_status, -1);
  atomic_init(&session->stop, false);
  atomic_init(&session->reader_done, false);
  atomic_init(&session->gen, 0);
  atomic_init(&session->worker_pool, NULL);
  atomic_init(&session->score_has_request, false);
  atomic_init(&session->score_growth_pending, false);
  atomic_init(&session->score_abort, false);
  atomic_init(&session->score_progress_completed, 0);
  atomic_init(&session->score_progress_total, 0);

  pthread_mutex_init(&session->mu, NULL);
  pthread_mutex_init(&session->child_mu, NULL);
  pthread_mutex_init(&session->score_req_mu, NULL);
  pthread_cond_init(&session->score_req_cond, NULL);
  pthread_mutex_init(&session->score_res_mu, NULL);
  session->pid = -1;

  cache_init_limits(&session->cache, 40, 64 * 1024 * 1024);
  batch_cache_init(&session->batch_cache, 64 * 1024 * 1024);

  /* A zero worker count leaves the session unpooled.  Cold-start probes use
     this mode so their first request performs the process's first worker
     creation, matching production startup rather than warming pthread state
     with a throwaway private pool. */
  if (workers == 0) return session;

  struct AsyncWorkerPool *pool = async_worker_pool_create(workers);
  if (!pool) {
    async_session_destroy(session);
    return NULL;
  }
  atomic_store_explicit(&session->worker_pool, pool, memory_order_release);
  session->worker_pool_owned = true;
  return session;
}

static bool bench_start_coordinator(AsyncSession *session) {
  if (pthread_create(&session->score_thread, NULL,
                     scoring_thread_fn, session) != 0)
    return false;
  session->score_thread_started = true;
  return true;
}

static bool bench_append_candidate(AsyncSession *session, size_t ordinal) {
  char candidate[64];
  int length = snprintf(candidate, sizeof candidate,
                        "candidate-%010zu-test-file", ordinal);
  return length > 0 && (size_t)length < sizeof candidate &&
      async_append_candidate(session, candidate, (size_t)length);
}

static bool bench_append_range(AsyncSession *session, size_t first,
                               size_t count) {
  if (count > SIZE_MAX - first) return false;
  for (size_t i = 0; i < count; i++)
    if (!bench_append_candidate(session, first + i)) return false;
  return true;
}

static bool bench_wait_for_result(AsyncSession *session, uint64_t request_id,
                                  size_t pool_generation) {
  double deadline = bench_now_ms() + 30000.0;
  while (bench_now_ms() < deadline) {
    pthread_mutex_lock(&session->score_req_mu);
    bool request_idle = session->score_req_id == 0 &&
                        session->score_current_id == 0;
    pthread_mutex_unlock(&session->score_req_mu);

    pthread_mutex_lock(&session->score_res_mu);
    bool result_ready = session->score_result_id == request_id &&
                        session->score_result_pool_gen == pool_generation &&
                        session->score_error_id != request_id;
    pthread_mutex_unlock(&session->score_res_mu);

    bool growth_idle = !atomic_load_explicit(
        &session->score_growth_pending, memory_order_acquire);
    if (request_idle && result_ready && growth_idle) return true;
    bench_pause();
  }

  pthread_mutex_lock(&session->score_req_mu);
  uint64_t queued = session->score_req_id;
  uint64_t current = session->score_current_id;
  pthread_mutex_unlock(&session->score_req_mu);
  pthread_mutex_lock(&session->score_res_mu);
  uint64_t result = session->score_result_id;
  uint64_t error = session->score_error_id;
  size_t result_pool = session->score_result_pool_gen;
  pthread_mutex_unlock(&session->score_res_mu);
  fprintf(stderr,
          "timeout: request=%" PRIu64 " pool=%zu queued=%" PRIu64
          " current=%" PRIu64 " result=%" PRIu64
          " result-pool=%zu error=%" PRIu64 "\n",
          request_id, pool_generation, queued, current, result,
          result_pool, error);
  return false;
}

static bool bench_capture_snapshot(AsyncSession *session,
                                   uint64_t request_id,
                                   size_t pool_generation,
                                   BenchSnapshot *snapshot) {
  memset(snapshot, 0, sizeof *snapshot);
  pthread_mutex_lock(&session->score_res_mu);
  bool valid = session->score_result_id == request_id &&
               session->score_result_pool_gen == pool_generation &&
               session->score_error_id != request_id;
  if (valid) {
    snapshot->top_count = session->score_count;
    snapshot->matched_count = session->last_filtered;
    snapshot->pool_generation = session->score_result_pool_gen;
    if (snapshot->top_count) {
      snapshot->top = malloc(snapshot->top_count * sizeof *snapshot->top);
      if (!snapshot->top) {
        valid = false;
      } else {
        memcpy(snapshot->top, session->score_results,
               snapshot->top_count * sizeof *snapshot->top);
      }
    }
  }
  pthread_mutex_unlock(&session->score_res_mu);
  if (!valid) {
    free(snapshot->top);
    memset(snapshot, 0, sizeof *snapshot);
    return false;
  }
  snapshot->checksum = bench_snapshot_checksum(snapshot);
  return true;
}

static BenchMembershipStats bench_membership_stats(
    AsyncSession *session, const char *query) {
  SharedIdx *membership = NULL;
  size_t pool_generation = 0;
  BenchMembershipStats stats = {0};
  if (!cache_lookup_membership_exact(
          &session->cache, query, CaseSmart, true,
          false, true, FZF_SCORE_SCHEME_DEFAULT,
          &membership, &pool_generation))
    return stats;

  stats.available = true;
  stats.pool_generation = pool_generation;
  stats.matched_count = membership->count;
#ifdef SHARED_IDX_MAX_DEPTH
  stats.depth = membership->depth;
  stats.storage_bytes = membership->storage_bytes;
  stats.suffix_compactions_since_flatten =
      membership->suffix_compactions_since_flatten;
#else
  stats.depth = 1;
  stats.storage_bytes = sizeof *membership +
      membership->count * sizeof *membership->idx;
#endif
  shared_idx_release(membership);
  return stats;
}

/* Keep the benchmark oracle independent from the optimized radix sorter that
   produces session results.  This intentionally spells out fzf's public
   total order and uses libc qsort, so a defect in counting_sort_scored cannot
   validate itself. */
static int bench_compare_scored_total(const void *left_value,
                                      const void *right_value) {
  const ScoredStr *left = left_value;
  const ScoredStr *right = right_value;
  if (left->rank.score != right->rank.score)
    return left->rank.score > right->rank.score ? -1 : 1;
  if (left->rank.first != right->rank.first)
    return left->rank.first < right->rank.first ? -1 : 1;
  if (left->rank.second != right->rank.second)
    return left->rank.second < right->rank.second ? -1 : 1;
  if (left->idx != right->idx)
    return left->idx < right->idx ? -1 : 1;
  return 0;
}

static bool bench_validate_snapshot(AsyncSession *session,
                                    const BenchSnapshot *snapshot,
                                    const char *query, size_t limit,
                                    fzf_case_types case_mode, bool fuzzy,
                                    bool normalize, bool forward,
                                    fzf_score_scheme_t score_scheme) {
  if (snapshot->pool_generation > SIZE_MAX / sizeof(ScoredStr)) return false;
  ScoredStr *reference = snapshot->pool_generation
      ? malloc(snapshot->pool_generation * sizeof *reference) : NULL;
  if (snapshot->pool_generation && !reference) return false;

  char *query_copy = *query ? strdup(query) : NULL;
  if (*query && !query_copy) {
    free(reference);
    return false;
  }
  fzf_pattern_t *pattern = query_copy
      ? fzf_parse_pattern_with_direction(
            case_mode, normalize, query_copy, fuzzy, forward) : NULL;
  fzf_slab_t *slab = fzf_make_default_slab();
  if ((*query && !pattern) || !slab ||
      !fzf_slab_set_score_scheme(slab, score_scheme)) {
    free(query_copy);
    if (pattern) fzf_free_pattern(pattern);
    if (slab) fzf_free_slab(slab);
    free(reference);
    return false;
  }

  size_t matched = 0;
  bool can_reuse_public_score =
      fzf_rank_can_reuse_public_score(pattern, score_scheme);
  pthread_mutex_lock(&session->mu);
  for (size_t i = 0; i < snapshot->pool_generation; i++) {
    char *candidate = session->cands_top[i >> CANDS_BLOCK_SHIFT]
                                        [i & CANDS_BLOCK_MASK];
    FzfRankKeys rank = {0};
    size_t candidate_len = strlen(candidate);
    bool input_is_ascii = is_ascii_utf8proc(candidate, candidate_len);
    int score = pattern
        ? fzf_score_and_rank(
              candidate, candidate_len, input_is_ascii, pattern, slab,
              score_scheme, can_reuse_public_score, &rank)
        : 1;
    if (fzf_allocation_failed()) {
      pthread_mutex_unlock(&session->mu);
      free(query_copy);
      if (pattern) fzf_free_pattern(pattern);
      fzf_free_slab(slab);
      free(reference);
      return false;
    }
    if (score > 0)
      reference[matched++] = (ScoredStr){
          .str = candidate, .score = score, .idx = (uint32_t)i,
          .rank = rank};
  }
  pthread_mutex_unlock(&session->mu);

  /* Empty and inverse-only patterns preserve producer order. */
  if (pattern && pattern->has_positive_term && matched > 1)
    qsort(reference, matched, sizeof *reference,
          bench_compare_scored_total);
  size_t expected_count = limit && limit < matched ? limit : matched;
  BenchSnapshot expected_snapshot = {
      .top = reference,
      .top_count = expected_count,
      .matched_count = matched,
      .pool_generation = snapshot->pool_generation};
  uint64_t expected_checksum = bench_snapshot_checksum(&expected_snapshot);
  bool valid = matched == snapshot->matched_count &&
               expected_count == snapshot->top_count &&
               expected_checksum == snapshot->checksum;
  for (size_t i = 0; valid && i < expected_count; i++) {
    const ScoredStr *expected = &reference[i];
    const ScoredStr *actual = &snapshot->top[i];
    valid = expected->score == actual->score &&
            expected->idx == actual->idx &&
            strcmp(expected->str, actual->str) == 0;
    if (!valid)
      fprintf(stderr,
              "validation mismatch: pool=%zu rank=%zu "
              "expected={idx=%u score=%d str=%s} "
              "actual={idx=%u score=%d str=%s}\n",
              snapshot->pool_generation, i,
              expected->idx, expected->score, expected->str,
              actual->idx, actual->score, actual->str);
  }
  if (!valid && (matched != snapshot->matched_count ||
                 expected_count != snapshot->top_count ||
                 expected_checksum != snapshot->checksum))
    fprintf(stderr,
            "validation summary mismatch: pool=%zu "
            "expected-matched=%zu actual-matched=%zu "
            "expected-top=%zu actual-top=%zu "
            "expected-checksum=%016" PRIx64
            " actual-checksum=%016" PRIx64 "\n",
            snapshot->pool_generation, matched, snapshot->matched_count,
            expected_count, snapshot->top_count,
            expected_checksum, snapshot->checksum);

  if (valid)
    printf("validate pool=%zu matched=%zu top=%zu checksum=%016" PRIx64
           " status=ok\n",
           snapshot->pool_generation, matched, expected_count,
           expected_checksum);

  free(query_copy);
  if (pattern) fzf_free_pattern(pattern);
  fzf_free_slab(slab);
  free(reference);
  return valid;
}

static int bench_compare_double(const void *left, const void *right) {
  double a = *(const double *)left;
  double b = *(const double *)right;
  return (a > b) - (a < b);
}

static bool bench_parse_size(const char *text, size_t *out) {
  if (!text || !*text || *text == '-') return false;
  errno = 0;
  char *end = NULL;
  uintmax_t value = strtoumax(text, &end, 10);
  if (errno || !end || *end || value > SIZE_MAX) return false;
  *out = (size_t)value;
  return true;
}

int main(int argc, char **argv) {
  size_t initial = 1000000;
  size_t delta = 1000;
  /* Cross the bounded-chain flatten horizon under the default burst workload
     so the summary includes both cheap linked growth and flatten tail cost. */
  size_t rounds = 40;
  size_t workers_size = 8;
  size_t limit = 10000;
  if (argc > 6 ||
      (argc > 1 && !bench_parse_size(argv[1], &initial)) ||
      (argc > 2 && !bench_parse_size(argv[2], &delta)) ||
      (argc > 3 && !bench_parse_size(argv[3], &rounds)) ||
      (argc > 4 && !bench_parse_size(argv[4], &workers_size)) ||
      (argc > 5 && !bench_parse_size(argv[5], &limit)) ||
      initial == 0 || delta == 0 || rounds == 0 || workers_size == 0 ||
      workers_size > ASYNC_WORKER_LIMIT ||
      rounds > (SIZE_MAX - initial) / delta ||
      initial + rounds * delta > UINT32_MAX ||
      initial + rounds * delta > CANDS_TOP_CAP * CANDS_BLOCK_SIZE) {
    fprintf(stderr,
            "usage: %s [initial [delta [rounds [workers [limit]]]]]\n",
            argv[0]);
    return 2;
  }

  const char *query = "t";
  unsigned workers = (unsigned)workers_size;
  BenchSnapshot *snapshots = calloc(rounds + 1, sizeof *snapshots);
  double *latencies = calloc(rounds, sizeof *latencies);
  AsyncSession *session = bench_session_create(workers);
  if (!snapshots || !latencies || !session) {
    fprintf(stderr, "benchmark setup allocation failed\n");
    free(snapshots);
    free(latencies);
    if (session) async_session_destroy(session);
    return 3;
  }

  printf("config initial=%zu delta=%zu rounds=%zu workers=%u limit=%zu "
         "query=%s\n", initial, delta, rounds, workers, limit, query);
  if (!bench_append_range(session, 0, initial) ||
      !bench_start_coordinator(session)) {
    fprintf(stderr, "initial producer/session setup failed\n");
    async_session_destroy(session);
    free(snapshots);
    free(latencies);
    return 4;
  }

  double started = bench_now_ms();
  char *owned_query = strdup(query);
  uint64_t request_id = owned_query
      ? async_submit_request_resolved_for_scheme(
            session, owned_query, strlen(query), limit,
            CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT,
            0, false)
      : 0;
  if (!request_id ||
      !bench_wait_for_result(session, request_id, initial) ||
      !bench_capture_snapshot(
          session, request_id, initial, &snapshots[0])) {
    fprintf(stderr, "initial scoring request failed\n");
    async_session_destroy(session);
    free(snapshots);
    free(latencies);
    return 5;
  }
  double initial_ms = bench_now_ms() - started;
  BenchMembershipStats previous_membership =
      bench_membership_stats(session, query);
  printf("initial pool=%zu latency_ms=%.3f matched=%zu top=%zu "
         "checksum=%016" PRIx64 " membership_depth=%zu "
         "membership_bytes=%zu suffix_compactions=%zu\n",
         initial, initial_ms, snapshots[0].matched_count,
         snapshots[0].top_count, snapshots[0].checksum,
         previous_membership.depth, previous_membership.storage_bytes,
         previous_membership.suffix_compactions_since_flatten);

  bool timed_ok = true;
  size_t observed_suffix_compaction_rounds = 0;
  size_t observed_full_flatten_rounds = 0;
  for (size_t round = 0; round < rounds; round++) {
    size_t old_pool = initial + round * delta;
    size_t new_pool = old_pool + delta;
    started = bench_now_ms();
    if (!bench_append_range(session, old_pool, delta) ||
        !bench_wait_for_result(session, request_id, new_pool)) {
      timed_ok = false;
      break;
    }
    latencies[round] = bench_now_ms() - started;
    if (!bench_capture_snapshot(
            session, request_id, new_pool, &snapshots[round + 1])) {
      timed_ok = false;
      break;
    }
    BenchMembershipStats membership =
        bench_membership_stats(session, query);
    bool membership_consistent = !membership.available ||
        (membership.pool_generation == new_pool &&
         membership.matched_count == snapshots[round + 1].matched_count);
    if (!membership_consistent) {
      fprintf(stderr,
              "membership mismatch: pool=%zu cache-pool=%zu "
              "matched=%zu cache-matched=%zu\n",
              new_pool, membership.pool_generation,
              snapshots[round + 1].matched_count,
              membership.matched_count);
      timed_ok = false;
      break;
    }
    bool observed_depth_drop = membership.available &&
        previous_membership.available &&
        membership.depth < previous_membership.depth;
    const char *compaction_kind = "none";
    if (observed_depth_drop) {
      if (membership.suffix_compactions_since_flatten > 0) {
        observed_suffix_compaction_rounds++;
        compaction_kind = "suffix";
      } else {
        observed_full_flatten_rounds++;
        compaction_kind = "full";
      }
    }
    printf("growth round=%zu pool=%zu latency_ms=%.3f matched=%zu top=%zu "
           "checksum=%016" PRIx64 " membership_depth=%zu "
           "membership_bytes=%zu suffix_compactions=%zu "
           "compaction_observed=%s\n",
           round + 1, new_pool, latencies[round],
           snapshots[round + 1].matched_count,
           snapshots[round + 1].top_count,
           snapshots[round + 1].checksum,
           membership.depth, membership.storage_bytes,
           membership.suffix_compactions_since_flatten,
           compaction_kind);
    previous_membership = membership;
  }

  bool validation_ok = timed_ok;
  if (timed_ok) {
    for (size_t i = 0; i <= rounds; i++) {
      if (!bench_validate_snapshot(
              session, &snapshots[i], query, limit,
              CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT)) {
        validation_ok = false;
        break;
      }
    }
  }

  if (timed_ok) {
    double *sorted = malloc(rounds * sizeof *sorted);
    if (!sorted) {
      validation_ok = false;
    } else {
      memcpy(sorted, latencies, rounds * sizeof *sorted);
      qsort(sorted, rounds, sizeof *sorted, bench_compare_double);
      double total = 0.0;
      for (size_t i = 0; i < rounds; i++) total += latencies[i];
      double median = rounds & 1
          ? sorted[rounds / 2]
          : (sorted[rounds / 2 - 1] + sorted[rounds / 2]) / 2.0;
      size_t p95_rank = rounds - rounds / 20;
      printf("summary initial_ms=%.3f growth_total_ms=%.3f "
             "growth_mean_ms=%.3f growth_median_ms=%.3f "
             "growth_p95_ms=%.3f growth_max_ms=%.3f "
             "suffix_compaction_rounds=%zu full_flatten_rounds=%zu "
             "validation=%s snapshots=%zu\n",
             initial_ms, total, total / rounds, median,
             sorted[p95_rank - 1], sorted[rounds - 1],
             observed_suffix_compaction_rounds,
             observed_full_flatten_rounds,
             validation_ok ? "ok" : "failed", rounds + 1);
      free(sorted);
    }
  }

  async_session_destroy(session);
  for (size_t i = 0; i <= rounds; i++) free(snapshots[i].top);
  free(snapshots);
  free(latencies);
  return validation_ok ? 0 : 6;
}
