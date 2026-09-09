// SPDX-License-Identifier: MIT
/*
 * Single-process cold first-query probe for the persistent session.
 *
 * Run this executable once per sample: the process-wide worker pool is meant
 * to be uninitialized when the coordinator starts.  Corpus construction is
 * outside the timed interval, matching interactive sessions that receive
 * their first query after the producer has populated the candidate pool.
 */

#define main fzf_native_embedded_session_growth_benchmark_main
#include "../etc/session-growth-benchmark.c"
#undef main

typedef enum {
  FirstQueryAscii,
  FirstQueryArabic,
  FirstQueryKorean,
} FirstQueryCorpus;

static bool first_query_append_candidate(AsyncSession *session,
                                         AsyncLineDecoder *line,
                                         size_t ordinal,
                                         FirstQueryCorpus corpus) {
  char candidate[160];
  int length;
  if (corpus == FirstQueryArabic) {
    switch (ordinal % 4) {
      case 0:
        length = snprintf(candidate, sizeof candidate,
                          "workspace/إنشاء/ملف_%08zu.txt", ordinal);
        break;
      case 1:
        length = snprintf(candidate, sizeof candidate,
                          "workspace/مشروع/اختبار_%08zu.c", ordinal);
        break;
      case 2:
        length = snprintf(candidate, sizeof candidate,
                          "workspace/مكتبة/بيانات_%08zu.el", ordinal);
        break;
      default:
        length = snprintf(candidate, sizeof candidate,
                          "misc/عنصر_%08zu_payload.bin", ordinal);
        break;
    }
    return length > 0 && (size_t)length < sizeof candidate &&
           async_line_feed_bytes(session, line, candidate, (size_t)length) &&
           async_line_finish(session, line);
  }
  if (corpus == FirstQueryKorean) {
    switch (ordinal % 4) {
      case 0:
        length = snprintf(candidate, sizeof candidate,
                          "workspace/프로젝트/합니다_%08zu.txt", ordinal);
        break;
      case 1:
        length = snprintf(candidate, sizeof candidate,
                          "workspace/소스/테스트_%08zu.c", ordinal);
        break;
      case 2:
        length = snprintf(candidate, sizeof candidate,
                          "workspace/라이브러리/데이터_%08zu.el", ordinal);
        break;
      default:
        length = snprintf(candidate, sizeof candidate,
                          "misc/항목_%08zu_payload.bin", ordinal);
        break;
    }
    return length > 0 && (size_t)length < sizeof candidate &&
           async_line_feed_bytes(session, line, candidate, (size_t)length) &&
           async_line_finish(session, line);
  }
  switch (ordinal % 8) {
    case 0:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/omega_pipeline_%08zu.go", ordinal);
      break;
    case 1:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/lib/gamma_worker_%08zu.rs", ordinal);
      break;
    case 2:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/docs/delta_notes_%08zu.md", ordinal);
      break;
    case 3:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/tests/fuzzy_native_%08zu.test", ordinal);
      break;
    case 4:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/http_request_%08zu.ts", ordinal);
      break;
    case 5:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/src/alpha_component_%08zu.c", ordinal);
      break;
    case 6:
      length = snprintf(candidate, sizeof candidate,
                        "workspace/lib/algebra_table_%08zu.el", ordinal);
      break;
    default:
      length = snprintf(candidate, sizeof candidate,
                        "misc/item_%08zu_payload.bin", ordinal);
      break;
  }
  return length > 0 && (size_t)length < sizeof candidate &&
         async_line_feed_bytes(session, line, candidate, (size_t)length) &&
         async_line_finish(session, line);
}

static AsyncSession *first_query_create_session(size_t candidate_count,
                                                FirstQueryCorpus corpus) {
  /* Reuse the benchmark's complete session initializer, then remove its
     private pool before starting the coordinator.  Production sessions also
     begin with worker_pool == NULL and acquire the process-wide pool. */
  AsyncSession *session = bench_session_create(1);
  if (!session) return NULL;
  struct AsyncWorkerPool *private_pool = atomic_exchange_explicit(
      &session->worker_pool, NULL, memory_order_acq_rel);
  session->worker_pool_owned = false;
  async_worker_pool_destroy(private_pool);

  if (!bench_start_coordinator(session)) {
    async_session_destroy(session);
    return NULL;
  }
  AsyncLineDecoder line = {0};
  for (size_t i = 0; i < candidate_count; i++) {
    if (!first_query_append_candidate(session, &line, i, corpus)) {
      free(line.output);
      async_session_destroy(session);
      return NULL;
    }
  }
  free(line.output);
  return session;
}

int main(int argc, char **argv) {
  size_t candidate_count = 300000;
  size_t limit = 256;
  size_t batch_cache_bytes = 64 * 1024 * 1024;
  const char *query = "omega";
  FirstQueryCorpus corpus = FirstQueryAscii;
  if (argc > 6 ||
      (argc > 1 && !bench_parse_size(argv[1], &candidate_count)) ||
      (argc > 2 && !bench_parse_size(argv[2], &limit)) ||
      (argc > 4 && !bench_parse_size(argv[4], &batch_cache_bytes)) ||
      candidate_count == 0 || limit == 0) {
    fprintf(stderr,
            "usage: %s [candidates [limit [query [batch-cache-bytes "
            "[ascii|arabic|korean]]]]]\n",
            argv[0]);
    return 2;
  }
  if (argc > 3) query = argv[3];
  if (argc > 5) {
    if (strcmp(argv[5], "arabic") == 0)
      corpus = FirstQueryArabic;
    else if (strcmp(argv[5], "korean") == 0)
      corpus = FirstQueryKorean;
    else if (strcmp(argv[5], "ascii") != 0) {
      fprintf(stderr, "unknown corpus: %s\n", argv[5]);
      return 2;
    }
  }
  const char *corpus_name = corpus == FirstQueryArabic ? "arabic" :
                            corpus == FirstQueryKorean ? "korean" : "ascii";

  double setup_started = bench_now_ms();
  AsyncSession *session = first_query_create_session(candidate_count, corpus);
  double setup_ms = bench_now_ms() - setup_started;
  if (!session) return 3;
  session->batch_cache.max_bytes = batch_cache_bytes;
  char *owned_query = strdup(query);
  if (!owned_query) {
    async_session_destroy(session);
    return 3;
  }

  double started = bench_now_ms();
  uint64_t request_id = async_submit_request_resolved_for_scheme(
      session, owned_query, strlen(query), limit,
      CaseSmart, true, false, true, FZF_SCORE_SCHEME_DEFAULT, 0, false);
  bool complete = request_id &&
      bench_wait_for_result(session, request_id, candidate_count);
  double elapsed_ms = bench_now_ms() - started;

  BenchSnapshot snapshot = {0};
  bool valid = complete &&
      bench_capture_snapshot(session, request_id, candidate_count, &snapshot) &&
      bench_validate_snapshot(session, &snapshot, query, limit,
                              CaseSmart, true, false, true,
                              FZF_SCORE_SCHEME_DEFAULT);
  printf("first_query_ms=%.3f setup_ms=%.3f candidates=%zu corpus=%s "
         "query=%s matched=%zu top=%zu batch_cache_bytes=%zu "
         "checksum=%016" PRIx64 " validation=%s\n",
         elapsed_ms, setup_ms, candidate_count, corpus_name, query,
         snapshot.matched_count, snapshot.top_count, batch_cache_bytes, snapshot.checksum,
         valid ? "ok" : "failed");
  free(snapshot.top);
  async_session_destroy(session);
  return valid ? 0 : 4;
}
