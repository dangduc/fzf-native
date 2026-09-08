// SPDX-License-Identifier: MIT
/*
 * Deterministic persistent-session benchmark for interactive query edits.
 *
 * The validation pass compares every session result with an independent full
 * scan and qsort before any timing starts.  Timed samples use fresh sessions,
 * replay the same edit trace, and compare their result fingerprints with the
 * validated pass after each measurement.
 */

#define main fzf_native_embedded_session_growth_benchmark_main
#include "../etc/session-growth-benchmark.c"
#undef main

#ifndef TRACE_DEFAULT_CANDIDATES
#define TRACE_DEFAULT_CANDIDATES 100000u
#endif
#ifndef TRACE_DEFAULT_SAMPLES
#define TRACE_DEFAULT_SAMPLES 9u
#endif

typedef enum {
  TRACE_NARROW,
  TRACE_WIDEN,
  TRACE_UNRELATED,
} TraceEdit;

typedef struct {
  const char *query;
  TraceEdit edit;
  const char *label;
} TraceStep;

typedef struct {
  size_t matched_count;
  size_t top_count;
  uint64_t checksum;
} TraceFingerprint;

static const TraceStep trace_steps[] = {
    {"a", TRACE_NARROW, "narrow"},
    {"al", TRACE_NARROW, "narrow"},
    {"alp", TRACE_NARROW, "narrow"},
    {"alph", TRACE_NARROW, "narrow"},
    {"alpha", TRACE_NARROW, "narrow"},
    {"alph", TRACE_WIDEN, "backspace"},
    {"alp", TRACE_WIDEN, "backspace"},
    {"al", TRACE_WIDEN, "backspace"},
    {"om", TRACE_UNRELATED, "unrelated"},
    {"ome", TRACE_NARROW, "narrow"},
    {"omega", TRACE_NARROW, "narrow"},
    {"gam", TRACE_UNRELATED, "unrelated"},
    {"gamma", TRACE_NARROW, "narrow"},
    {"gma", TRACE_UNRELATED, "unrelated"},
};

#define TRACE_STEP_COUNT \
  (sizeof trace_steps / sizeof trace_steps[0])

static bool trace_append_candidate(AsyncSession *session, size_t ordinal) {
  char candidate[160];
  int length;
  switch (ordinal % 16) {
    case 0:
    case 1:
    case 2:
    case 3:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/alpha_component_%08zu_gamma_tail.c",
                        ordinal);
      break;
    case 4:
    case 5:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/alpine_adapter_%08zu.cc", ordinal);
      break;
    case 6:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/lib/algebra_table_%08zu.el", ordinal);
      break;
    case 7:
    case 8:
    case 9:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/gamma_worker_%08zu_alpha_hook.rs",
                        ordinal);
      break;
    case 10:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/omega_pipeline_%08zu.go", ordinal);
      break;
    case 11:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/docs/delta_notes_%08zu.md", ordinal);
      break;
    case 12:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/http_request_%08zu.ts", ordinal);
      break;
    case 13:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/tests/fuzzy_native_%08zu.test", ordinal);
      break;
    case 14:
      length = snprintf(candidate, sizeof candidate,
                        "WORKSPACE/SRC/ALPHA_COMPONENT_%08zu.H", ordinal);
      break;
    default:
      length = snprintf(candidate, sizeof candidate,
                        "misc/item_%08zu_payload.bin", ordinal);
      break;
  }
  return length > 0 && (size_t)length < sizeof candidate &&
         async_append_candidate(
             session, candidate, (size_t)length);
}

static bool trace_append_corpus(AsyncSession *session, size_t candidate_count) {
  for (size_t i = 0; i < candidate_count; i++)
    if (!trace_append_candidate(session, i)) return false;
  return true;
}

static AsyncSession *trace_create_session(size_t candidate_count,
                                          unsigned int workers) {
  AsyncSession *session = bench_session_create(workers);
  if (!session) return NULL;
  if (!trace_append_corpus(session, candidate_count) ||
      !bench_start_coordinator(session)) {
    async_session_destroy(session);
    return NULL;
  }
  return session;
}

static bool trace_submit_and_capture(AsyncSession *session,
                                     size_t candidate_count,
                                     size_t limit,
                                     const TraceStep *step,
                                     BenchSnapshot *snapshot,
                                     double *latency_ms) {
  char *query = strdup(step->query);
  if (!query) return false;
  double started = bench_now_ms();
  uint64_t request_id = async_submit_request_resolved_for_scheme(
      session, query, strlen(step->query), limit,
      CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT,
      0, false);
  if (!request_id ||
      !bench_wait_for_result(session, request_id, candidate_count))
    return false;
  if (latency_ms) *latency_ms = bench_now_ms() - started;
  return bench_capture_snapshot(
      session, request_id, candidate_count, snapshot);
}

static TraceFingerprint trace_fingerprint(const BenchSnapshot *snapshot) {
  return (TraceFingerprint){
      .matched_count = snapshot->matched_count,
      .top_count = snapshot->top_count,
      .checksum = snapshot->checksum,
  };
}

static bool trace_fingerprint_equal(TraceFingerprint left,
                                    TraceFingerprint right) {
  return left.matched_count == right.matched_count &&
         left.top_count == right.top_count &&
         left.checksum == right.checksum;
}

static void trace_snapshot_free(BenchSnapshot *snapshot) {
  free(snapshot->top);
  memset(snapshot, 0, sizeof *snapshot);
}

static bool trace_validate_relations(const TraceFingerprint *fingerprints) {
  for (size_t i = 1; i <= 4; i++)
    if (fingerprints[i].matched_count > fingerprints[i - 1].matched_count)
      return false;
  for (size_t i = 5; i <= 7; i++)
    if (fingerprints[i].matched_count < fingerprints[i - 1].matched_count)
      return false;

  const size_t repeated[][2] = {{3, 5}, {2, 6}, {1, 7}};
  for (size_t i = 0; i < sizeof repeated / sizeof repeated[0]; i++)
    if (!trace_fingerprint_equal(fingerprints[repeated[i][0]],
                                 fingerprints[repeated[i][1]]))
      return false;

  const size_t unrelated[] = {8, 11, 13};
  for (size_t i = 0; i < sizeof unrelated / sizeof unrelated[0]; i++) {
    size_t step = unrelated[i];
    if (trace_fingerprint_equal(fingerprints[step - 1],
                                fingerprints[step]))
      return false;
  }
  return true;
}

static bool trace_validation_pass(size_t candidate_count,
                                  unsigned int workers, size_t limit,
                                  TraceFingerprint *expected) {
  AsyncSession *session = trace_create_session(candidate_count, workers);
  if (!session) return false;

  bool valid = true;
  for (size_t i = 0; i < TRACE_STEP_COUNT; i++) {
    BenchSnapshot snapshot = {0};
    if (!trace_submit_and_capture(session, candidate_count, limit,
                                  &trace_steps[i], &snapshot, NULL) ||
        !bench_validate_snapshot(
            session, &snapshot, trace_steps[i].query, limit,
            CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT)) {
      valid = false;
      trace_snapshot_free(&snapshot);
      break;
    }
    expected[i] = trace_fingerprint(&snapshot);
    printf("validate step=%zu edit=%s query=%s matched=%zu top=%zu "
           "fingerprint=%016" PRIx64 " status=ok\n",
           i + 1, trace_steps[i].label, trace_steps[i].query,
           snapshot.matched_count, snapshot.top_count, snapshot.checksum);
    trace_snapshot_free(&snapshot);
  }
  if (valid) valid = trace_validate_relations(expected);
  async_session_destroy(session);
  return valid;
}

static int trace_compare_double(const void *left_value,
                                const void *right_value) {
  double left = *(const double *)left_value;
  double right = *(const double *)right_value;
  return (left > right) - (left < right);
}

static double trace_median(double *values, size_t count) {
  qsort(values, count, sizeof *values, trace_compare_double);
  return values[count / 2];
}

static bool trace_timed_sample(size_t candidate_count,
                               unsigned int workers, size_t limit,
                               const TraceFingerprint *expected,
                               double *step_latencies) {
  AsyncSession *session = trace_create_session(candidate_count, workers);
  if (!session) return false;
  bool valid = true;
  for (size_t i = 0; i < TRACE_STEP_COUNT; i++) {
    BenchSnapshot snapshot = {0};
    if (!trace_submit_and_capture(
            session, candidate_count, limit, &trace_steps[i],
            &snapshot, &step_latencies[i]) ||
        !trace_fingerprint_equal(trace_fingerprint(&snapshot), expected[i])) {
      valid = false;
      trace_snapshot_free(&snapshot);
      break;
    }
    trace_snapshot_free(&snapshot);
  }
  async_session_destroy(session);
  return valid;
}

int main(int argc, char **argv) {
  size_t candidate_count = TRACE_DEFAULT_CANDIDATES;
  size_t workers_size = 8;
  size_t limit = 256;
  size_t samples = TRACE_DEFAULT_SAMPLES;
  if (argc > 5 ||
      (argc > 1 && !bench_parse_size(argv[1], &candidate_count)) ||
      (argc > 2 && !bench_parse_size(argv[2], &workers_size)) ||
      (argc > 3 && !bench_parse_size(argv[3], &limit)) ||
      (argc > 4 && !bench_parse_size(argv[4], &samples)) ||
      candidate_count == 0 || workers_size == 0 ||
      workers_size > ASYNC_WORKER_LIMIT || limit == 0 || samples == 0 ||
      (samples & 1) == 0) {
    fprintf(stderr,
            "usage: %s [candidates [workers [limit [odd-samples]]]]\n",
            argv[0]);
    return 2;
  }

  unsigned int workers = (unsigned int)workers_size;
  TraceFingerprint expected[TRACE_STEP_COUNT] = {0};
  printf("config candidates=%zu workers=%u limit=%zu samples=%zu steps=%zu\n",
         candidate_count, workers, limit, samples, TRACE_STEP_COUNT);
  if (!trace_validation_pass(candidate_count, workers, limit, expected)) {
    fprintf(stderr, "session trace validation failed before timing\n");
    return 3;
  }
  puts("validation status=ok timing=begin");

  if (samples > SIZE_MAX / TRACE_STEP_COUNT ||
      samples * TRACE_STEP_COUNT > SIZE_MAX / sizeof(double))
    return 4;
  double *latencies = calloc(
      samples * TRACE_STEP_COUNT, sizeof *latencies);
  if (!latencies) return 4;
  for (size_t sample = 0; sample < samples; sample++) {
    if (!trace_timed_sample(
            candidate_count, workers, limit, expected,
            latencies + sample * TRACE_STEP_COUNT)) {
      fprintf(stderr, "timed fingerprint mismatch at sample=%zu\n",
              sample + 1);
      free(latencies);
      return 5;
    }
  }

  double *scratch = malloc(samples * sizeof *scratch);
  double *trace_totals = calloc(samples, sizeof *trace_totals);
  double *narrow_totals = calloc(samples, sizeof *narrow_totals);
  double *widen_totals = calloc(samples, sizeof *widen_totals);
  double *unrelated_totals = calloc(samples, sizeof *unrelated_totals);
  if (!scratch || !trace_totals || !narrow_totals || !widen_totals ||
      !unrelated_totals) {
    free(scratch);
    free(trace_totals);
    free(narrow_totals);
    free(widen_totals);
    free(unrelated_totals);
    free(latencies);
    return 4;
  }

  for (size_t step = 0; step < TRACE_STEP_COUNT; step++) {
    for (size_t sample = 0; sample < samples; sample++) {
      double value = latencies[sample * TRACE_STEP_COUNT + step];
      scratch[sample] = value;
      trace_totals[sample] += value;
      if (trace_steps[step].edit == TRACE_NARROW)
        narrow_totals[sample] += value;
      else if (trace_steps[step].edit == TRACE_WIDEN)
        widen_totals[sample] += value;
      else
        unrelated_totals[sample] += value;
    }
    printf("timing step=%zu edit=%s query=%s median_ms=%.3f\n",
           step + 1, trace_steps[step].label, trace_steps[step].query,
           trace_median(scratch, samples));
  }

  printf("summary trace_median_ms=%.3f narrow_median_ms=%.3f "
         "backspace_median_ms=%.3f unrelated_median_ms=%.3f "
         "validation=ok samples=%zu\n",
         trace_median(trace_totals, samples),
         trace_median(narrow_totals, samples),
         trace_median(widen_totals, samples),
         trace_median(unrelated_totals, samples), samples);

  free(scratch);
  free(trace_totals);
  free(narrow_totals);
  free(widen_totals);
  free(unrelated_totals);
  free(latencies);
  return 0;
}
