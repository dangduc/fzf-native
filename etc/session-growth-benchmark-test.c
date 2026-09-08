/* SPDX-License-Identifier: GPL-3.0-or-later
 * Regression test for the persistent-session benchmark oracle.
 *
 * Include the benchmark in this translation unit so its private fixture and
 * validation helpers remain the code under test.
 */

#define main fzf_native_session_growth_benchmark_main
#include "session-growth-benchmark.c"
#undef main

int main(void) {
  AsyncSession *session = bench_session_create(2);
  if (!session) return 1;

  /* Producer order is deliberately different from final fzf rank order. */
  const char *candidates[] = {
    "xxxxxxxxxxxxxxxxxxxxxt",
    "t",
    "zzzt",
    "test",
  };
  for (size_t i = 0; i < sizeof candidates / sizeof candidates[0]; i++) {
    if (!async_append_candidate(
            session, candidates[i], strlen(candidates[i]))) {
      async_session_destroy(session);
      return 1;
    }
  }
  if (!bench_start_coordinator(session)) {
    async_session_destroy(session);
    return 1;
  }

  char *query = strdup("t");
  uint64_t request_id = query
      ? async_submit_request_resolved_for_scheme(
            session, query, 1, 4, CaseSmart, true, false, true,
            FZF_SCORE_SCHEME_DEFAULT, 0, false)
      : 0;
  BenchSnapshot snapshot = {0};
  static const uint32_t expected_ranked[] = {1, 3, 2, 0};
  bool valid = request_id &&
      bench_wait_for_result(session, request_id, 4) &&
      bench_capture_snapshot(session, request_id, 4, &snapshot) &&
      snapshot.top_count ==
          sizeof expected_ranked / sizeof expected_ranked[0];
  for (size_t i = 0; valid && i < snapshot.top_count; i++)
    valid = snapshot.top[i].idx == expected_ranked[i];
  valid = valid &&
      bench_validate_snapshot(
          session, &snapshot, "t", 4,
          CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT);
  free(snapshot.top);

  /* An inverse-only query has no positive score to rank.  fzf therefore
     keeps the surviving candidates in producer order. */
  char *inverse_query = strdup("!x");
  request_id = inverse_query
      ? async_submit_request_resolved_for_scheme(
            session, inverse_query, 2, 4, CaseSmart, true, false, true,
            FZF_SCORE_SCHEME_DEFAULT, 0, false)
      : 0;
  snapshot = (BenchSnapshot){0};
  static const uint32_t expected_inverse[] = {1, 2, 3};
  bool inverse_valid = request_id &&
      bench_wait_for_result(session, request_id, 4) &&
      bench_capture_snapshot(session, request_id, 4, &snapshot) &&
      snapshot.top_count ==
          sizeof expected_inverse / sizeof expected_inverse[0];
  for (size_t i = 0; inverse_valid && i < snapshot.top_count; i++)
    inverse_valid = snapshot.top[i].idx == expected_inverse[i];
  inverse_valid = inverse_valid &&
      bench_validate_snapshot(
          session, &snapshot, "!x", 4,
          CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT);
  free(snapshot.top);
  valid = valid && inverse_valid;

  async_session_destroy(session);
  if (!valid) {
    fprintf(stderr, "session-growth benchmark rank oracle failed\n");
    return 1;
  }
  puts("session-growth benchmark rank oracle passed");
  return 0;
}
