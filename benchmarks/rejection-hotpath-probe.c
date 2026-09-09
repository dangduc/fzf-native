// SPDX-License-Identifier: MIT
/*
 * Deterministic microbenchmark for rejection-heavy ASCII score calls.
 *
 * This intentionally uses synthetic candidates rather than either external
 * benchmark suite.  It isolates the scorer's mandatory-subsequence reject,
 * exact-substring reject, and their match-heavy counterweights.
 *
 * Build from the repository root:
 *
 *   cc -std=gnu11 -O3 -DNDEBUG -I. -Iutf8proc-2.10.0 \
 *      -o build/rejection-hotpath-probe \
 *      benchmarks/rejection-hotpath-probe.c fzf.c \
 *      utf8proc-2.10.0/utf8proc.c
 */

#include "fzf.h"
#include "fzf-private.h"

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ITEM_COUNT 32768u
#ifndef PROBE_SAMPLES
#define PROBE_SAMPLES 15u
#endif

typedef enum {
  SHAPE_EARLY_MISS,
  SHAPE_TERMINAL_MISS,
  SHAPE_RARE_MISS,
  SHAPE_GUARD_PRESENT_MISS,
  SHAPE_PARTIAL_MIX,
  SHAPE_ALL_MATCH,
} candidate_shape_t;

typedef enum {
  API_PRECLASSIFIED,
  API_BOUNDED,
  API_C_STRING,
} score_api_t;

typedef struct {
  char *bytes;
  size_t length;
} candidate_t;

typedef struct {
  uint64_t scores;
  uint64_t membership;
} fingerprint_t;

static volatile uint64_t benchmark_sink;

static uint64_t now_ns(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) abort();
  return (uint64_t)ts.tv_sec * UINT64_C(1000000000) +
         (uint64_t)ts.tv_nsec;
}

static int compare_u64(const void *left, const void *right) {
  uint64_t a = *(const uint64_t *)left;
  uint64_t b = *(const uint64_t *)right;
  return (a > b) - (a < b);
}

static uint64_t mix64(uint64_t value) {
  value ^= value >> 30;
  value *= UINT64_C(0xbf58476d1ce4e5b9);
  value ^= value >> 27;
  value *= UINT64_C(0x94d049bb133111eb);
  return value ^ (value >> 31);
}

static void place_spaced(char *bytes, size_t length,
                         const char *sequence, size_t sequence_length) {
  for (size_t i = 0; i < sequence_length; i++) {
    size_t at = sequence_length == 1
                    ? 0
                    : i * (length - 1) / (sequence_length - 1);
    bytes[at] = sequence[i];
  }
}

static void apply_shape(char *bytes, size_t length, size_t item_index,
                        candidate_shape_t shape) {
  static const char query[] = "deadbeef";
  if (shape == SHAPE_PARTIAL_MIX) {
    size_t kind = item_index % 20;
    shape = kind == 0 ? SHAPE_ALL_MATCH
                      : (kind < 5 ? SHAPE_TERMINAL_MISS
                                  : SHAPE_EARLY_MISS);
  }
  if (shape == SHAPE_EARLY_MISS) {
    /* The terminal guard succeeds, then the first required d is absent. */
    bytes[length - 1] = 'f';
  } else if (shape == SHAPE_TERMINAL_MISS) {
    /* Seven ordered query bytes followed by a guaranteed missing f. */
    place_spaced(bytes, length, "deadbee", 7);
  } else if (shape == SHAPE_RARE_MISS) {
    /* The generic-frequency rarest query byte, b, is absent; f is present. */
    place_spaced(bytes, length, "deadeef", 7);
  } else if (shape == SHAPE_GUARD_PRESENT_MISS) {
    /* Every distinct query byte is present, but order rejects the match. */
    place_spaced(bytes, length, "feebdaed", 8);
  } else {
    size_t at = (item_index * 7) % (length - (sizeof query - 1) + 1);
    memcpy(bytes + at, query, sizeof query - 1);
  }
}

static candidate_t *make_candidates(size_t length, candidate_shape_t shape) {
  static const char filler[] = "acghijklmnopqstuvwxyz0123456789_/-";
  candidate_t *items = calloc(ITEM_COUNT, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < ITEM_COUNT; i++) {
    char *bytes = malloc(length + 1);
    if (!bytes) abort();
    for (size_t j = 0; j < length; j++)
      bytes[j] = filler[(i * 13 + j * 17) % (sizeof filler - 1)];

    apply_shape(bytes, length, i, shape);
    bytes[length] = '\0';
    items[i] = (candidate_t){bytes, length};
  }
  return items;
}

static candidate_t *make_repeated_candidates(size_t length, bool matches) {
  static const char filler[] = "cghijklmnopqstuvwxyz0123456789_/-";
  candidate_t *items = calloc(ITEM_COUNT, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < ITEM_COUNT; i++) {
    char *bytes = malloc(length + 1);
    if (!bytes) abort();
    for (size_t j = 0; j < length; j++)
      bytes[j] = filler[(i * 13 + j * 17) % (sizeof filler - 1)];
    if (matches) place_spaced(bytes, length, "aAaAaAaA", 8);
    bytes[length] = '\0';
    items[i] = (candidate_t){bytes, length};
  }
  return items;
}

static void free_candidates(candidate_t *items) {
  for (size_t i = 0; i < ITEM_COUNT; i++) free(items[i].bytes);
  free(items);
}

static int32_t score_item(const candidate_t *item, fzf_pattern_t *pattern,
                          fzf_slab_t *slab, score_api_t api) {
  if (api == API_PRECLASSIFIED)
    return fzf_get_score_bytes_preclassified(
        item->bytes, item->length, true, pattern, slab);
  if (api == API_BOUNDED)
    return fzf_get_score_bytes(item->bytes, item->length, pattern, slab);
  return fzf_get_score(item->bytes, pattern, slab);
}

static fingerprint_t fingerprint(const candidate_t *items,
                                 fzf_pattern_t *pattern,
                                 fzf_slab_t *slab, score_api_t api) {
  fingerprint_t result = {
      UINT64_C(0x243f6a8885a308d3), UINT64_C(0x13198a2e03707344)};
  for (size_t i = 0; i < ITEM_COUNT; i++) {
    int32_t score = score_item(&items[i], pattern, slab, api);
    if (fzf_allocation_failed()) abort();
    result.scores = mix64(result.scores ^ (uint32_t)score ^ mix64(i));
    result.membership = mix64(result.membership +
                              (score > 0 ? UINT64_C(0x9e3779b97f4a7c15)
                                         : UINT64_C(0xd1b54a32d192ed03)) +
                              i);
  }
  return result;
}

static uint64_t time_once(const candidate_t *items, fzf_pattern_t *pattern,
                          fzf_slab_t *slab, score_api_t api) {
  uint64_t checksum = 0;
  uint64_t start = now_ns();
  for (size_t i = 0; i < ITEM_COUNT; i++) {
    checksum += (uint32_t)score_item(&items[i], pattern, slab, api);
    if (fzf_allocation_failed()) abort();
  }
  uint64_t elapsed = now_ns() - start;
  benchmark_sink += checksum;
  return elapsed;
}

static double median_ns_per_item(const candidate_t *items,
                                 fzf_pattern_t *pattern,
                                 fzf_slab_t *slab, score_api_t api) {
  for (size_t i = 0; i < 3; i++) (void)time_once(items, pattern, slab, api);
  uint64_t samples[PROBE_SAMPLES];
  for (size_t i = 0; i < PROBE_SAMPLES; i++)
    samples[i] = time_once(items, pattern, slab, api);
  qsort(samples, PROBE_SAMPLES, sizeof samples[0], compare_u64);
  return (double)samples[PROBE_SAMPLES / 2] / ITEM_COUNT;
}

static void measure_case(candidate_t *items, size_t length,
                         const char *shape_name, char *query, bool fuzzy,
                         fzf_case_types case_mode) {
  fzf_pattern_t *pattern = fzf_parse_pattern(
      case_mode, false, query, fuzzy);
  fzf_slab_t *slab = fzf_make_default_slab();
  if (!pattern || !slab) abort();

  fingerprint_t identity = fingerprint(
      items, pattern, slab, API_PRECLASSIFIED);
  fingerprint_t bounded = fingerprint(items, pattern, slab, API_BOUNDED);
  fingerprint_t c_string = fingerprint(items, pattern, slab, API_C_STRING);
  if (identity.scores != bounded.scores ||
      identity.membership != bounded.membership ||
      identity.scores != c_string.scores ||
      identity.membership != c_string.membership) {
    fprintf(stderr, "score API fingerprints differ\n");
    abort();
  }
  double preclassified = median_ns_per_item(
      items, pattern, slab, API_PRECLASSIFIED);
  double bounded_ns = median_ns_per_item(
      items, pattern, slab, API_BOUNDED);
  double c_string_ns = median_ns_per_item(
      items, pattern, slab, API_C_STRING);
  printf("%-5s %-7s len=%-3zu %-14s pre=%7.3f bounded=%7.3f "
         "cstr=%7.3f ns/item fp=%016" PRIx64
         "%016" PRIx64 "\n",
         fuzzy ? "fuzzy" : "exact",
         case_mode == CaseRespect ? "respect" : "ignore", length,
         shape_name, preclassified, bounded_ns, c_string_ns,
         identity.scores, identity.membership);

  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
  free_candidates(items);
}

static void run_case(size_t length, candidate_shape_t shape, bool fuzzy,
                     fzf_case_types case_mode) {
  static const char *shape_names[] = {
      "early-miss", "terminal-miss", "rare-miss", "all-bytes-miss",
      "partial-mix", "all-match"};
  char query[] = "deadbeef";
  measure_case(make_candidates(length, shape), length, shape_names[shape],
               query, fuzzy, case_mode);
}

static void run_repeated_case(size_t length, bool matches) {
  char query[] = "aaaaaaaa";
  measure_case(make_repeated_candidates(length, matches), length,
               matches ? "repeat-match" : "repeat-miss", query, true,
               CaseIgnore);
}

int main(void) {
  static const size_t lengths[] = {16, 32, 64, 128};
  for (size_t fuzzy = 0; fuzzy < 2; fuzzy++)
    for (size_t shape = 0; shape < 6; shape++)
      for (size_t li = 0; li < sizeof lengths / sizeof lengths[0]; li++)
        run_case(lengths[li], (candidate_shape_t)shape, fuzzy != 0,
                 CaseIgnore);
  run_case(32, SHAPE_TERMINAL_MISS, true, CaseRespect);
  run_case(128, SHAPE_TERMINAL_MISS, true, CaseRespect);
  run_case(32, SHAPE_ALL_MATCH, true, CaseRespect);
  run_case(128, SHAPE_ALL_MATCH, true, CaseRespect);
  for (size_t matches = 0; matches < 2; matches++)
    for (size_t li = 0; li < sizeof lengths / sizeof lengths[0]; li++)
      run_repeated_case(lengths[li], matches != 0);
  printf("sink=%" PRIu64 "\n", benchmark_sink);
  return 0;
}
