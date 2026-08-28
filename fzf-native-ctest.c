/* Standalone C unit tests for fzf-native-module internals.
 *
 * This file #includes fzf-native-module.c directly so that `static`
 * functions like counting_sort_candidates and cmp_candidate are visible.
 * Build and run via `make ctest`.
 *
 * The test does not invoke any Emacs runtime APIs — it only exercises
 * pure-C functions that operate on plain data. emacs_value globals from
 * the module file end up zero-initialized in the test binary; that's fine
 * because no test path dereferences them.
 */

#include <stdatomic.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static _Atomic size_t ctest_strdup_calls;
static _Atomic size_t ctest_strdup_fail_at;
static char *ctest_strdup(const char *source);

/* Route module-local strdup calls through a normally transparent wrapper so
   request-publication tests can fault one exact ownership copy. */
#define strdup ctest_strdup
#define FZF_NATIVE_CTEST 1
#include "fzf-native-module.c"
#undef strdup

#include <assert.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static char *ctest_strdup(const char *source) {
  size_t call = atomic_fetch_add_explicit(
      &ctest_strdup_calls, 1, memory_order_relaxed) + 1;
  if (call == atomic_load_explicit(
                  &ctest_strdup_fail_at, memory_order_relaxed))
    return NULL;
  size_t length = strlen(source) + 1;
  char *copy = malloc(length);
  if (copy) memcpy(copy, source, length);
  return copy;
}

static int failed = 0;
#define CHECK(cond) do {                                                \
    if (!(cond)) {                                                      \
      fprintf(stderr, "  FAIL %s:%d: %s\n", __FILE__, __LINE__, #cond); \
      failed++;                                                         \
    }                                                                   \
  } while (0)

#define RUN(name) do { printf("RUN  %s\n", #name); name(); } while (0)

/* Small helper to build a Candidate with a given score and an order tag
   carried in s.len (we never dereference s.b in these tests). */
static struct Candidate make_candidate(int score, size_t tag) {
  struct Candidate c;
  memset(&c, 0, sizeof c);
  c.score = score;
  c.s.len = tag;
  return c;
}

static int is_descending_by_score(struct Candidate *xs, size_t n) {
  for (size_t i = 1; i < n; i++)
    if (xs[i - 1].score < xs[i].score) return 0;
  return 1;
}

static void test_session_abi_is_versioned(void) {
  CHECK(FZF_NATIVE_SESSION_ABI == 1);
}

static void test_async_snapshot_staleness_covers_pool_growth(void) {
  /* A completed result is stale whenever it describes an older append-only
     pool boundary.  This pure predicate is deterministic; an Emacs poller
     cannot reliably catch the short interval before the automatic growth
     retry publishes its replacement. */
  CHECK(async_snapshot_is_stale(7, 7, 1, 2, false));
  CHECK(async_snapshot_is_stale(7, 7, 1, 2, true));
  CHECK(!async_snapshot_is_stale(7, 7, 2, 2, false));
  CHECK(async_snapshot_is_stale(7, 6, 2, 2, true));
  CHECK(async_snapshot_is_stale(7, 7, 0, 0, false));
  CHECK(!async_snapshot_is_stale(7, 7, 0, 0, true));
}

static void test_batch_worker_stops_on_shared_allocation_failure(void) {
  struct Shared allocation = {
    .pattern = NULL,
    .batches = NULL,
    .remaining = 0,
    .filter_only = false,
    .allocation_failed = false,
    .embedded_nul = false,
  };
  CHECK(!shared_worker_should_stop(&allocation));
  shared_set_embedded_nul(&allocation);
  CHECK(shared_embedded_nul(&allocation));
  /* Invalid input is reported after join, but must not restore the ordinary
     per-candidate cross-worker load that regressed ASCII scoring. */
  CHECK(!shared_worker_should_stop(&allocation));
  shared_set_allocation_failed(&allocation);
  CHECK(shared_worker_should_stop(&allocation));
}

static enum emacs_funcall_exit ctest_copy_pending_exit;
static emacs_value ctest_copy_original;
static emacs_value ctest_copy_encoded;
static bool ctest_copy_fail_original;
static bool ctest_copy_fail_encoded;
static size_t ctest_encode_calls;

static bool ctest_copy_string_contents(emacs_env *env, emacs_value value,
                                       char *buffer, ptrdiff_t *length) {
  (void)env;
  if ((value == ctest_copy_original && ctest_copy_fail_original) ||
      (value == ctest_copy_encoded && ctest_copy_fail_encoded)) {
    ctest_copy_pending_exit = emacs_funcall_exit_signal;
    return false;
  }
  if (!buffer) {
    *length = 2;
    return true;
  }
  if (*length < 2) return false;
  buffer[0] = 'x';
  buffer[1] = '\0';
  *length = 2;
  return true;
}

static enum emacs_funcall_exit ctest_copy_exit_check(emacs_env *env) {
  (void)env;
  return ctest_copy_pending_exit;
}

static void ctest_copy_exit_clear(emacs_env *env) {
  (void)env;
  ctest_copy_pending_exit = emacs_funcall_exit_return;
}

static emacs_value ctest_encode_string(emacs_env *env, emacs_value function,
                                       ptrdiff_t nargs, emacs_value *args) {
  (void)env;
  (void)function;
  (void)nargs;
  (void)args;
  ctest_encode_calls++;
  return ctest_copy_encoded;
}

static void ctest_reset_copy_env(void) {
  ctest_copy_pending_exit = emacs_funcall_exit_return;
  ctest_copy_original = (emacs_value)(uintptr_t)0x31;
  ctest_copy_encoded = (emacs_value)(uintptr_t)0x32;
  ctest_copy_fail_original = false;
  ctest_copy_fail_encoded = false;
  ctest_encode_calls = 0;
}

static void test_copy_emacs_string_fallback_is_bounded(void) {
  emacs_env env = {0};
  env.copy_string_contents = ctest_copy_string_contents;
  env.non_local_exit_check = ctest_copy_exit_check;
  env.non_local_exit_clear = ctest_copy_exit_clear;
  env.funcall = ctest_encode_string;

  /* A native allocation failure is not a conversion failure.  It must return
     directly without invoking encode-coding-string or recursing. */
  ctest_reset_copy_env();
  atomic_store_explicit(&copy_test_fail_bump_allocation, true,
                        memory_order_release);
  struct Bump *bump = NULL;
  struct Str copied = copy_emacs_string(&env, &bump, ctest_copy_original);
  CHECK(copied.b == NULL);
  CHECK(bump == NULL);
  CHECK(ctest_encode_calls == 0);

  /* A direct conversion signal gets exactly one encoded retry. */
  ctest_reset_copy_env();
  ctest_copy_fail_original = true;
  copied = copy_emacs_string(&env, &bump, ctest_copy_original);
  CHECK(copied.b != NULL);
  CHECK(copied.len == 1);
  CHECK(copied.b[0] == 'x');
  CHECK(ctest_encode_calls == 1);
  bump_free(bump);
  bump = NULL;

  /* If the encoded value also fails, return after that one attempt. */
  ctest_reset_copy_env();
  ctest_copy_fail_original = true;
  ctest_copy_fail_encoded = true;
  copied = copy_emacs_string(&env, &bump, ctest_copy_original);
  CHECK(copied.b == NULL);
  CHECK(bump == NULL);
  CHECK(ctest_encode_calls == 1);
  CHECK(ctest_copy_pending_exit == emacs_funcall_exit_return);
}

static enum emacs_funcall_exit ctest_string_pending_exit;
static bool ctest_make_string_should_signal;
static size_t ctest_make_string_calls;
static size_t ctest_make_unibyte_string_calls;
static size_t ctest_non_local_exit_clear_calls;

static emacs_value ctest_make_string(emacs_env *env, const char *bytes,
                                     ptrdiff_t length) {
  (void)env;
  (void)bytes;
  (void)length;
  ctest_make_string_calls++;
  if (ctest_make_string_should_signal)
    ctest_string_pending_exit = emacs_funcall_exit_signal;
  return (emacs_value)(uintptr_t)0x11;
}

static emacs_value ctest_make_unibyte_string(emacs_env *env,
                                             const char *bytes,
                                             ptrdiff_t length) {
  (void)env;
  (void)bytes;
  (void)length;
  ctest_make_unibyte_string_calls++;
  return (emacs_value)(uintptr_t)0x22;
}

static enum emacs_funcall_exit ctest_non_local_exit_check(emacs_env *env) {
  (void)env;
  return ctest_string_pending_exit;
}

static void ctest_non_local_exit_clear(emacs_env *env) {
  (void)env;
  ctest_non_local_exit_clear_calls++;
  ctest_string_pending_exit = emacs_funcall_exit_return;
}

static void ctest_reset_string_env(void) {
  ctest_string_pending_exit = emacs_funcall_exit_return;
  ctest_make_string_should_signal = false;
  ctest_make_string_calls = 0;
  ctest_make_unibyte_string_calls = 0;
  ctest_non_local_exit_clear_calls = 0;
}

static void test_lossless_string_conversion_preserves_runtime_errors(void) {
  emacs_env env = {0};
  env.make_string = ctest_make_string;
  env.make_unibyte_string = ctest_make_unibyte_string;
  env.non_local_exit_check = ctest_non_local_exit_check;
  env.non_local_exit_clear = ctest_non_local_exit_clear;

  /* A signal from make_string can be memory-full or another runtime failure.
     Valid UTF-8 must retain that pending exit, not clear it and retry through
     a representation-changing unibyte path. */
  ctest_reset_string_env();
  ctest_make_string_should_signal = true;
  emacs_value valid = async_make_lisp_string_lossless(&env, "valid", 5);
  CHECK((uintptr_t)valid == 0x11);
  CHECK(ctest_make_string_calls == 1);
  CHECK(ctest_make_unibyte_string_calls == 0);
  CHECK(ctest_non_local_exit_clear_calls == 0);
  CHECK(ctest_string_pending_exit == emacs_funcall_exit_signal);

  /* Invalid bytes are classified before the module call.  They go directly
     to make_unibyte_string and therefore require no catch-all signal clear. */
  ctest_reset_string_env();
  const char invalid[] = {(char)0xff};
  emacs_value raw = async_make_lisp_string_lossless(&env, invalid, 1);
  CHECK((uintptr_t)raw == 0x22);
  CHECK(ctest_make_string_calls == 0);
  CHECK(ctest_make_unibyte_string_calls == 1);
  CHECK(ctest_non_local_exit_clear_calls == 0);
  CHECK(ctest_string_pending_exit == emacs_funcall_exit_return);
}

static char ctest_signalled_message[256];
static bool ctest_signal_called;

static emacs_value ctest_capture_make_string(emacs_env *env,
                                             const char *bytes,
                                             ptrdiff_t length) {
  (void)env;
  size_t copied = length > 0 ? (size_t)length : 0;
  if (copied >= sizeof ctest_signalled_message)
    copied = sizeof ctest_signalled_message - 1;
  memcpy(ctest_signalled_message, bytes, copied);
  ctest_signalled_message[copied] = '\0';
  return (emacs_value)(uintptr_t)0x33;
}

static emacs_value ctest_identity_funcall(emacs_env *env, emacs_value fn,
                                          ptrdiff_t nargs,
                                          emacs_value *args) {
  (void)env;
  (void)fn;
  return nargs > 0 ? args[0] : (emacs_value)0;
}

static void ctest_capture_signal(emacs_env *env, emacs_value symbol,
                                 emacs_value data) {
  (void)env;
  (void)symbol;
  (void)data;
  ctest_signal_called = true;
}

static void test_startup_posix_errors_are_descriptive(void) {
  emacs_env env = {0};
  env.make_string = ctest_capture_make_string;
  env.funcall = ctest_identity_funcall;
  env.non_local_exit_signal = ctest_capture_signal;
  ctest_signal_called = false;
  ctest_signalled_message[0] = '\0';

  async_signal_posix_error(&env, "producer status pipe creation", EMFILE);

  CHECK(ctest_signal_called);
  CHECK(strstr(ctest_signalled_message,
               "producer status pipe creation failed") != NULL);
  CHECK(strstr(ctest_signalled_message, strerror(EMFILE)) != NULL);
}

static size_t ctest_make_user_ptr_calls;

static emacs_value ctest_fail_make_user_ptr(emacs_env *env,
                                            emacs_finalizer finalizer,
                                            void *pointer) {
  (void)env;
  (void)finalizer;
  CHECK(pointer == NULL);
  ctest_make_user_ptr_calls++;
  ctest_string_pending_exit = emacs_funcall_exit_signal;
  return NULL;
}

static void test_async_start_publishes_inert_handle_before_resources(void) {
  emacs_env env = {0};
  env.make_user_ptr = ctest_fail_make_user_ptr;
  env.non_local_exit_check = ctest_non_local_exit_check;
  ctest_make_user_ptr_calls = 0;
  ctest_string_pending_exit = emacs_funcall_exit_return;
  emacs_value args[] = {(emacs_value)(uintptr_t)0x44};

  emacs_value result = fzf_native_async_start(&env, 1, args, NULL);

  CHECK(result == Qnil);
  CHECK(ctest_make_user_ptr_calls == 1);
  CHECK(ctest_string_pending_exit == emacs_funcall_exit_signal);
}

static void test_status_metadata_does_not_copy_results(void) {
  AsyncSession s;
  memset(&s, 0, sizeof s);
  pthread_mutex_init(&s.score_res_mu, NULL);
  s.score_count = 10000000;
  /* Deliberately no score_results allocation: metadata-only copying must
     retain the count without touching or allocating the candidate array. */
  size_t count = 0, limit = 0;
  size_t progress_completed = 0, progress_total = 0;
  size_t filtered = 0, total = 0;
  uint64_t snapshot_generation = 0, error_id = 0;
  AsyncResultObservation result_observation = {0};
  char *filter = NULL, *error = NULL;
  fzf_case_types case_mode = CaseSmart;
  bool fuzzy = false, filter_only = false, allocation_failed = false;
  ScoredStr *copy = async_copy_public_result(
      &s, false, &count, &result_observation, &filter, &limit, &case_mode,
      &fuzzy, &filter_only, &snapshot_generation,
      &progress_completed, &progress_total, &error_id, &error,
      &filtered, &total, &allocation_failed);
  CHECK(copy == NULL);
  CHECK(count == 10000000);
  CHECK(!allocation_failed);
  free(filter);
  free(error);
  pthread_mutex_destroy(&s.score_res_mu);
}

static void test_snapshot_capture_uses_live_pool_boundary(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.mu, NULL);
  pthread_mutex_init(&s.score_res_mu, NULL);
  s.count = 2;
  s.score_result_id = 7;
  s.score_result_pool_gen = 1;
  atomic_store_explicit(&s.reader_done, true, memory_order_release);

  size_t count = 0, limit = 0;
  size_t completed = 0, progress_total = 0, filtered = 0, source_total = 0;
  uint64_t generation = 0, error_id = 0;
  AsyncResultObservation result_observation = {0};
  char *filter = NULL, *error = NULL;
  fzf_case_types case_mode = CaseSmart;
  bool fuzzy = false, filter_only = false, allocation_failed = false;
  ScoredStr *copy = async_copy_public_result(
      &s, false, &count, &result_observation, &filter, &limit, &case_mode,
      &fuzzy, &filter_only, &generation,
      &completed, &progress_total, &error_id, &error,
      &filtered, &source_total, &allocation_failed);
  AsyncSnapshotState state = async_capture_snapshot_state(
      &s, 7, result_observation);

  CHECK(copy == NULL);
  CHECK(!allocation_failed);
  CHECK(state.result.request_id == 7);
  CHECK(state.result.pool_generation == 1);
  CHECK(state.pool.count == 2);
  CHECK(state.pool.reader_done);
  CHECK(state.stale);
  free(filter);
  free(error);

  s.score_result_pool_gen = 2;
  filter = NULL;
  error = NULL;
  allocation_failed = false;
  copy = async_copy_public_result(
      &s, false, &count, &result_observation, &filter, &limit, &case_mode,
      &fuzzy, &filter_only, &generation,
      &completed, &progress_total, &error_id, &error,
      &filtered, &source_total, &allocation_failed);
  state = async_capture_snapshot_state(&s, 7, result_observation);
  CHECK(copy == NULL);
  CHECK(!allocation_failed);
  CHECK(state.result.pool_generation == 2);
  CHECK(!state.stale);
  free(filter);
  free(error);

  pthread_mutex_destroy(&s.score_res_mu);
  pthread_mutex_destroy(&s.mu);
}

static void test_snapshot_copy_oom_is_not_authoritative_empty(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.score_res_mu, NULL);
  ScoredStr result = {.str = "alpha", .score = 10, .idx = 0};
  s.score_results = &result;
  s.score_count = 1;
  s.score_result_id = 9;
  s.score_result_filter = "a";
  s.last_filtered = 1;
  s.last_total = 1;

  size_t count = 0, limit = 0;
  size_t completed = 0, total = 0, filtered = 0, source_total = 0;
  uint64_t generation = 0, error_id = 0;
  AsyncResultObservation result_observation = {0};
  char *filter = NULL, *error = NULL;
  fzf_case_types case_mode = CaseSmart;
  bool fuzzy = false, filter_only = false, allocation_failed = false;
  atomic_store_explicit(&async_test_fail_result_copy_allocation, true,
                        memory_order_release);
  ScoredStr *copy = async_copy_public_result(
      &s, true, &count, &result_observation, &filter, &limit, &case_mode,
      &fuzzy, &filter_only, &generation,
      &completed, &total, &error_id, &error,
      &filtered, &source_total, &allocation_failed);

  CHECK(copy == NULL);
  CHECK(allocation_failed);
  CHECK(count == 0);
  CHECK(result_observation.request_id == 9);
  CHECK(filtered == 1 && source_total == 1);
  free(filter);
  free(error);
  pthread_mutex_destroy(&s.score_res_mu);
}

static void test_snapshot_copy_owns_candidate_strings(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.score_res_mu, NULL);
  char source[] = "alpha";
  ScoredStr result = {.str = source, .score = 10, .idx = 3};
  s.score_results = &result;
  s.score_count = 1;

  size_t count = 0, limit = 0;
  size_t completed = 0, total = 0, filtered = 0, source_total = 0;
  uint64_t generation = 0, error_id = 0;
  AsyncResultObservation result_observation = {0};
  char *filter = NULL, *error = NULL;
  fzf_case_types case_mode = CaseSmart;
  bool fuzzy = false, filter_only = false, allocation_failed = false;
  ScoredStr *copy = async_copy_public_result(
      &s, true, &count, &result_observation, &filter, &limit, &case_mode,
      &fuzzy, &filter_only, &generation,
      &completed, &total, &error_id, &error,
      &filtered, &source_total, &allocation_failed);

  CHECK(copy != NULL);
  CHECK(count == 1);
  CHECK(!allocation_failed);
  if (copy) {
    CHECK(copy[0].str != source);
    source[0] = 'z';
    CHECK(strcmp(copy[0].str, "alpha") == 0);
    CHECK(copy[0].score == 10);
    CHECK(copy[0].idx == 3);
  }
  async_free_public_result(copy, count);
  free(filter);
  free(error);
  pthread_mutex_destroy(&s.score_res_mu);
}

static void test_snapshot_string_copy_oom_is_not_authoritative_empty(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.score_res_mu, NULL);
  ScoredStr result = {.str = "alpha", .score = 10, .idx = 0};
  s.score_results = &result;
  s.score_count = 1;

  size_t count = 0, limit = 0;
  size_t completed = 0, total = 0, filtered = 0, source_total = 0;
  uint64_t generation = 0, error_id = 0;
  AsyncResultObservation result_observation = {0};
  char *filter = NULL, *error = NULL;
  fzf_case_types case_mode = CaseSmart;
  bool fuzzy = false, filter_only = false, allocation_failed = false;
  size_t next_copy = atomic_load_explicit(
      &ctest_strdup_calls, memory_order_relaxed) + 1;
  atomic_store_explicit(&ctest_strdup_fail_at, next_copy,
                        memory_order_relaxed);
  ScoredStr *copy = async_copy_public_result(
      &s, true, &count, &result_observation, &filter, &limit, &case_mode,
      &fuzzy, &filter_only, &generation,
      &completed, &total, &error_id, &error,
      &filtered, &source_total, &allocation_failed);
  atomic_store_explicit(&ctest_strdup_fail_at, 0, memory_order_relaxed);

  CHECK(copy == NULL);
  CHECK(count == 0);
  CHECK(allocation_failed);
  async_free_public_result(copy, count);
  free(filter);
  free(error);
  pthread_mutex_destroy(&s.score_res_mu);
}

static void test_growth_retry_query_oom_is_terminal(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.mu, NULL);
  pthread_mutex_init(&s.score_req_mu, NULL);
  pthread_mutex_init(&s.score_res_mu, NULL);
  s.score_latest_id = 17;
  s.score_latest_filter = ctest_strdup("needle");
  s.count = 2;
  s.score_result_id = 17;
  s.score_result_pool_gen = 1;
  atomic_store_explicit(&s.score_growth_pending, true,
                        memory_order_release);

  size_t next_copy = atomic_load_explicit(
      &ctest_strdup_calls, memory_order_relaxed) + 1;
  atomic_store_explicit(&ctest_strdup_fail_at, next_copy,
                        memory_order_relaxed);
  bool queued = async_queue_growth_retry(&s);
  atomic_store_explicit(&ctest_strdup_fail_at, 0, memory_order_relaxed);

  CHECK(!queued);
  CHECK(!atomic_load_explicit(&s.score_growth_pending,
                              memory_order_acquire));
  CHECK(s.score_req_id == 0);
  CHECK(s.score_current_id == 0);
  CHECK(s.score_error_id == 17);
  CHECK(s.score_error != NULL);
  CHECK(strstr(s.score_error, "growth retry query") != NULL);
  CHECK(async_request_state(17, s.score_latest_id, s.score_req_id,
                            s.score_current_id, s.score_result_id,
                            s.score_error_id) == AsyncRequestFailed);
  CHECK(atomic_load_explicit(&s.gen, memory_order_relaxed) == 1);

  free(s.score_latest_filter);
  free(s.score_error);
  pthread_mutex_destroy(&s.score_res_mu);
  pthread_mutex_destroy(&s.score_req_mu);
  pthread_mutex_destroy(&s.mu);
}

/* --- Tests --- */

static void test_n_zero(void) {
  struct Candidate xs[1] = { make_candidate(7, 0) };
  counting_sort_candidates(xs, 0);
  CHECK(xs[0].score == 7);  /* untouched */
}

static void test_n_one(void) {
  struct Candidate xs[1] = { make_candidate(42, 0) };
  counting_sort_candidates(xs, 1);
  CHECK(xs[0].score == 42);
}

static void test_small_n_insertion_sort(void) {
  /* n=8 hits the n < 64 insertion-sort path. */
  struct Candidate xs[8] = {
    make_candidate(5, 0), make_candidate(3, 1), make_candidate(9, 2),
    make_candidate(1, 3), make_candidate(7, 4), make_candidate(0, 5),
    make_candidate(9, 6), make_candidate(4, 7),
  };
  counting_sort_candidates(xs, 8);
  CHECK(is_descending_by_score(xs, 8));
  CHECK(xs[0].score == 9);
  CHECK(xs[7].score == 0);
}

static void test_small_n_stability(void) {
  struct Candidate xs[8];
  for (size_t i = 0; i < 8; i++)
    xs[i] = make_candidate(7, i);
  counting_sort_candidates(xs, 8);
  for (size_t i = 0; i < 8; i++) {
    CHECK(xs[i].score == 7);
    CHECK(xs[i].s.len == i);
  }
}

static void test_large_n_correctness(void) {
  /* n=1000 takes the counting-sort path. Verify descending order. */
  enum { N = 1000 };
  struct Candidate *xs = malloc(N * sizeof *xs);
  CHECK(xs != NULL);
  unsigned seed = 0xC0FFEE;
  for (size_t i = 0; i < N; i++) {
    xs[i] = make_candidate((int)(rand_r(&seed) % 5000), i);
  }
  counting_sort_candidates(xs, N);
  CHECK(is_descending_by_score(xs, N));
  free(xs);
}

static void test_stability_with_ties(void) {
  /* Counting sort must be stable: same-score candidates retain input order.
     Use n>=64 so we go through the counting-sort path (qsort isn't stable
     and this property doesn't hold for the fallback). */
  enum { N = 200 };
  struct Candidate xs[N];
  for (size_t i = 0; i < N; i++) {
    /* score alternates 10 / 5; tag = original index */
    xs[i] = make_candidate((i % 2 == 0) ? 10 : 5, i);
  }
  counting_sort_candidates(xs, N);

  /* First N/2 entries: score=10, tags 0,2,4,... in order */
  for (size_t i = 0; i < N / 2; i++) {
    CHECK(xs[i].score == 10);
    CHECK(xs[i].s.len == i * 2);
  }
  /* Next N/2 entries: score=5, tags 1,3,5,... in order */
  for (size_t i = 0; i < N / 2; i++) {
    CHECK(xs[N / 2 + i].score == 5);
    CHECK(xs[N / 2 + i].s.len == i * 2 + 1);
  }
}

static void test_all_same_score(void) {
  /* All candidates same nonzero score -- single bucket. Verify stability:
     output order matches input order. */
  enum { N = 128 };
  struct Candidate xs[N];
  for (size_t i = 0; i < N; i++) xs[i] = make_candidate(7, i);
  counting_sort_candidates(xs, N);
  for (size_t i = 0; i < N; i++) {
    CHECK(xs[i].score == 7);
    CHECK(xs[i].s.len == i);
  }
}

static void test_all_zero_score(void) {
  /* score=0 is the only edge that touches max_score=0 -> count[1] alloc.
     The contract says callers ensure score >= 0; zero is allowed. */
  enum { N = 100 };
  struct Candidate xs[N];
  for (size_t i = 0; i < N; i++) xs[i] = make_candidate(0, i);
  counting_sort_candidates(xs, N);
  for (size_t i = 0; i < N; i++) {
    CHECK(xs[i].score == 0);
    CHECK(xs[i].s.len == i);
  }
}

static void test_matches_qsort(void) {
  /* For the same input, counting_sort and qsort should produce the same
     sequence of *scores* (the tags may diverge because qsort isn't stable). */
  enum { N = 500 };
  struct Candidate a[N], b[N];
  unsigned seed = 1234;
  for (size_t i = 0; i < N; i++) {
    int s = (int)(rand_r(&seed) % 1000);
    a[i] = make_candidate(s, i);
    b[i] = make_candidate(s, i);
  }
  counting_sort_candidates(a, N);
  qsort(b, N, sizeof *b, cmp_candidate);
  for (size_t i = 0; i < N; i++) CHECK(a[i].score == b[i].score);
}

/* =====================================================================
 * counting_sort_scored (async-path twin of counting_sort_candidates)
 * ===================================================================== */

/* Abuse the str pointer as an order tag; counting_sort_scored never
   dereferences str, only copies it, so this is safe for tests. */
static ScoredStr make_scored(int score, size_t tag) {
  ScoredStr s;
  s.str   = (char *)(uintptr_t)tag;
  s.score = score;
  s.idx   = (uint32_t)tag;
  return s;
}

static int is_scored_descending(ScoredStr *xs, size_t n) {
  for (size_t i = 1; i < n; i++)
    if (xs[i - 1].score < xs[i].score) return 0;
  return 1;
}

static void test_scored_n_zero(void) {
  ScoredStr xs[1] = { make_scored(7, 0) };
  counting_sort_scored(xs, 0);
  CHECK(xs[0].score == 7);
}

static void test_scored_n_one(void) {
  ScoredStr xs[1] = { make_scored(42, 0) };
  counting_sort_scored(xs, 1);
  CHECK(xs[0].score == 42);
}

static void test_scored_large_n_correctness(void) {
  enum { N = 1000 };
  ScoredStr *xs = malloc(N * sizeof *xs);
  CHECK(xs != NULL);
  unsigned seed = 0xBEEF;
  for (size_t i = 0; i < N; i++)
    xs[i] = make_scored((int)(rand_r(&seed) % 5000), i);
  counting_sort_scored(xs, N);
  CHECK(is_scored_descending(xs, N));
  free(xs);
}

static void test_scored_stability(void) {
  enum { N = 200 };
  ScoredStr xs[N];
  for (size_t i = 0; i < N; i++)
    xs[i] = make_scored((i % 2 == 0) ? 10 : 5, i);
  counting_sort_scored(xs, N);
  for (size_t i = 0; i < N / 2; i++) {
    CHECK(xs[i].score == 10);
    CHECK((size_t)(uintptr_t)xs[i].str == i * 2);
  }
  for (size_t i = 0; i < N / 2; i++) {
    CHECK(xs[N / 2 + i].score == 5);
    CHECK((size_t)(uintptr_t)xs[N / 2 + i].str == i * 2 + 1);
  }
}

static void test_scored_matches_qsort(void) {
  enum { N = 500 };
  ScoredStr a[N], b[N];
  unsigned seed = 4321;
  for (size_t i = 0; i < N; i++) {
    int s = (int)(rand_r(&seed) % 1000);
    a[i] = make_scored(s, i);
    b[i] = make_scored(s, i);
  }
  counting_sort_scored(a, N);
  qsort(b, N, sizeof *b, cmp_scored_desc);
  for (size_t i = 0; i < N; i++) CHECK(a[i].score == b[i].score);
}

static void test_bounded_top_k_matches_full_stable_sort(void) {
  enum { N = 2049, K = 73, SPLIT = 997 };
  ScoredStr input[N], reference[N], left[SPLIT], right[N - SPLIT], out[K];
  unsigned seed = 0x51A8E;
  for (size_t i = 0; i < N; i++) {
    input[i] = make_scored((int)(rand_r(&seed) % 17), i);
    reference[i] = input[i];
  }
  memcpy(left, input, sizeof left);
  memcpy(right, input + SPLIT, sizeof right);
  qsort(reference, N, sizeof *reference, cmp_scored_desc);
  counting_sort_scored(left, SPLIT);
  counting_sort_scored(right, N - SPLIT);
  size_t count = async_merge_top_k(
      left, MIN((size_t)K, (size_t)SPLIT),
      right, MIN((size_t)K, (size_t)(N - SPLIT)),
      K, out, NULL);
  CHECK(count == K);
  for (size_t i = 0; i < K; i++) {
    CHECK(out[i].score == reference[i].score);
    CHECK(out[i].idx == reference[i].idx);
  }
}

static void test_membership_cap_discards_incomplete_prefix(void) {
  AsyncMembershipBuilder builder = {.max_count = 2, .enabled = true};
  ScoredStr first[] = {{.idx = 3}, {.idx = 8}};
  ScoredStr overflow[] = {{.idx = 13}};
  async_membership_append(&builder, first, 2, NULL);
  CHECK(builder.enabled);
  CHECK(builder.count == 2);
  async_membership_append(&builder, overflow, 1, NULL);
  CHECK(!builder.enabled);
  CHECK(builder.idx == NULL);
  CHECK(builder.count == 0);
}

static void test_top_k_finalization_observes_cancellation(void) {
  ScoredStr left[] = {{.score = 9, .idx = 1}};
  ScoredStr right[] = {{.score = 8, .idx = 2}};
  ScoredStr out[2];
  _Atomic bool stop = true;
  CHECK(async_merge_top_k(left, 1, right, 1, 2, out, &stop) == SIZE_MAX);
  CHECK(!counting_sort_scored_abortable(right, 1, &stop));
}

static void test_allocationless_sort_matches_total_order(void) {
  enum { N = 4097 };
  ScoredStr *actual = malloc(N * sizeof *actual);
  ScoredStr *expected = malloc(N * sizeof *expected);
  CHECK(actual != NULL && expected != NULL);
  if (!actual || !expected) {
    free(actual);
    free(expected);
    return;
  }
  unsigned seed = 0xA110C;
  for (size_t i = 0; i < N; i++) {
    actual[i] = make_scored((int)(rand_r(&seed) % 31), i);
    expected[i] = actual[i];
  }
  qsort(expected, N, sizeof *expected, cmp_scored_desc);
  CHECK(async_heap_sort_scored(actual, N, NULL));
  for (size_t i = 0; i < N; i++) {
    CHECK(actual[i].score == expected[i].score);
    CHECK(actual[i].idx == expected[i].idx);
  }
  _Atomic bool stop = true;
  CHECK(!async_heap_sort_scored(actual, N, &stop));
  free(actual);
  free(expected);
}

/* =====================================================================
 * async_strip_ansi
 * ===================================================================== */

static void test_strip_ansi_no_escape(void) {
  char buf[] = "hello world";
  size_t n = async_strip_ansi(buf, strlen(buf));
  CHECK(n == 11);
  CHECK(strcmp(buf, "hello world") == 0);
}

static void test_strip_ansi_simple_color(void) {
  char buf[] = "\x1b[32mhello\x1b[0m";
  size_t n = async_strip_ansi(buf, strlen(buf));
  CHECK(n == 5);
  CHECK(strcmp(buf, "hello") == 0);
}

static void test_strip_ansi_multiple_sequences(void) {
  char buf[] = "\x1b[1m\x1b[31mfoo\x1b[0m";
  size_t n = async_strip_ansi(buf, strlen(buf));
  CHECK(n == 3);
  CHECK(strcmp(buf, "foo") == 0);
}

static void test_strip_ansi_bare_esc(void) {
  /* \x1b not followed by '[' must be preserved. */
  char buf[] = "\x1bhello";
  size_t orig = strlen(buf);
  size_t n = async_strip_ansi(buf, orig);
  CHECK(n == orig);
  CHECK(buf[0] == '\x1b');
}

/* =====================================================================
 * async_reader (pipe-based; no Emacs runtime needed)
 * ===================================================================== */

/* The `cap` parameter is ignored under the chunked-storage design — the
   reader allocates blocks lazily.  Kept in the signature so existing test
   sites remain readable without rewrites. */
static AsyncSession *make_async_session(FILE *fp, size_t cap) {
  (void)cap;
  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  s->fp = fp;
  s->pid = -1;
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->child_mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  return s;
}

static void free_async_session(AsyncSession *s) {
  arena_free(&s->arena);
  for (size_t k = 0; k < CANDS_TOP_CAP; k++)
    if (s->cands_top[k]) free(s->cands_top[k]);
  pthread_cond_destroy(&s->score_req_cond);
  pthread_mutex_destroy(&s->score_res_mu);
  pthread_mutex_destroy(&s->score_req_mu);
  pthread_mutex_destroy(&s->child_mu);
  pthread_mutex_destroy(&s->mu);
  free(s);
}

/* Convenience accessor: read s->cands_top[i >> SHIFT][i & MASK].
   Returns NULL if the block isn't allocated (which would be a bug). */
static const char *cands_at(AsyncSession *s, size_t i) {
  char **block = s->cands_top[i >> CANDS_BLOCK_SHIFT];
  return block ? block[i & CANDS_BLOCK_MASK] : NULL;
}

static void test_async_reader_basic(void) {
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  FILE *wfp = fdopen(pfd[1], "w");
  CHECK(wfp != NULL);
  fprintf(wfp, "alpha\nbeta\ngamma\n");
  fclose(wfp);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);

  async_reader((void *)s);

  CHECK(s->count == 3);
  CHECK(strcmp(cands_at(s, 0), "alpha") == 0);
  CHECK(strcmp(cands_at(s, 1), "beta")  == 0);
  CHECK(strcmp(cands_at(s, 2), "gamma") == 0);
  CHECK(!atomic_load_explicit(&s->score_growth_pending,
                              memory_order_acquire));
  free_async_session(s);
}

static void test_async_reader_coalesces_growth_after_request(void) {
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  FILE *wfp = fdopen(pfd[1], "w");
  CHECK(wfp != NULL);
  fprintf(wfp, "alpha\nbeta\ngamma\n");
  fclose(wfp);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);
  atomic_store_explicit(&s->score_has_request, true, memory_order_release);

  async_reader((void *)s);

  CHECK(s->count == 3);
  CHECK(atomic_load_explicit(&s->score_growth_pending,
                             memory_order_acquire));
  free_async_session(s);
}

static void test_async_reader_publishes_terminal_generation(void) {
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  close(pfd[1]);
  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);
  s->score_snapshot_generation = 7;
  atomic_store_explicit(&s->gen, 11, memory_order_relaxed);

  async_reader((void *)s);

  CHECK(atomic_load_explicit(&s->reader_done, memory_order_acquire));
  CHECK(atomic_load_explicit(&s->producer_state,
                             memory_order_acquire) == AsyncProducerComplete);
  CHECK(s->score_snapshot_generation == 8);
  CHECK(atomic_load_explicit(&s->gen, memory_order_relaxed) == 12);
  free_async_session(s);
}

static void test_async_reader_ansi_stripping(void) {
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  FILE *wfp = fdopen(pfd[1], "w");
  CHECK(wfp != NULL);
  fprintf(wfp, "\x1b[32mfile.txt\x1b[0m\n");
  fprintf(wfp, "plain.c\n");
  fclose(wfp);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);

  async_reader((void *)s);

  CHECK(s->count == 2);
  CHECK(strcmp(cands_at(s, 0), "file.txt") == 0);
  CHECK(strcmp(cands_at(s, 1), "plain.c")  == 0);
  free_async_session(s);
}

static void test_async_reader_preserves_empty_records(void) {
  static const char output[] =
      "\nalpha\n\n\r\n\x1b[31m\x1b[0m\nomega\n\n";
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  CHECK(write(pfd[1], output, sizeof output - 1) ==
        (ssize_t)(sizeof output - 1));
  close(pfd[1]);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);

  async_reader((void *)s);

  static const char *expected[] = {
      "", "alpha", "", "", "", "omega", "",
  };
  CHECK(s->count == sizeof expected / sizeof expected[0]);
  for (size_t i = 0; i < sizeof expected / sizeof expected[0]; i++) {
    CHECK(cands_at(s, i) != NULL);
    CHECK(strcmp(cands_at(s, i), expected[i]) == 0);
  }
  free_async_session(s);
}

static void test_async_reader_many_lines(void) {
  /* Write 32 lines.  Under the chunked-storage design no realloc is
     involved; this just verifies sequential round-trip through the
     accessor. */
  enum { NLINES = 32 };
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  FILE *wfp = fdopen(pfd[1], "w");
  CHECK(wfp != NULL);
  for (int i = 0; i < NLINES; i++) fprintf(wfp, "line%d\n", i);
  fclose(wfp);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 4);
  CHECK(s != NULL);

  async_reader((void *)s);

  CHECK(s->count == (size_t)NLINES);
  char expected[32];
  for (int i = 0; i < NLINES; i++) {
    snprintf(expected, sizeof expected, "line%d", i);
    CHECK(strcmp(cands_at(s, i), expected) == 0);
  }
  /* All 32 fit in block 0; later blocks must be untouched. */
  CHECK(s->cands_top[0] != NULL);
  CHECK(s->cands_top[1] == NULL);
  free_async_session(s);
}

/* Pre-getline, the reader used `fgets' into a fixed 8 KB stack buffer and
   chopped any line longer than that into 8 KB shards at arbitrary I/O
   boundaries.  After the switch to getline, a single 20 KB logical line
   must arrive as exactly one candidate of length 20000.  Regression guard
   for the fragmentation bug. */
static void test_async_reader_long_line(void) {
  enum { LINE_LEN = 20000 };
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  FILE *wfp = fdopen(pfd[1], "w");
  CHECK(wfp != NULL);
  for (int i = 0; i < LINE_LEN; i++) fputc('x', wfp);
  fputc('\n', wfp);
  fclose(wfp);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);
  /* max_line_length=0 (calloc default) → unbounded mode; the long line
     is delivered intact rather than excluded by the user-facing cap. */

  async_reader((void *)s);

  CHECK(s->count == 1);
  const char *cand = cands_at(s, 0);
  CHECK(cand != NULL);
  CHECK(strlen(cand) == (size_t)LINE_LEN);
  /* Sanity: first/last bytes match what we wrote, no I/O-boundary garbage. */
  CHECK(cand[0]            == 'x');
  CHECK(cand[LINE_LEN - 1] == 'x');
  free_async_session(s);
}

static void test_async_reader_final_unterminated_line(void) {
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  CHECK(write(pfd[1], "alpha\nomega", 11) == 11);
  close(pfd[1]);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);

  async_reader((void *)s);

  CHECK(s->count == 2);
  CHECK(strcmp(cands_at(s, 0), "alpha") == 0);
  CHECK(strcmp(cands_at(s, 1), "omega") == 0);
  free_async_session(s);
}

static void test_async_reader_rejects_embedded_nul(void) {
  static const char output[] = {
    'v', 'a', 'l', 'i', 'd', '\n',
    'a', 'b', '\0', 'c', 'd', '\n',
    'l', 'a', 't', 'e', '\n',
  };
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  CHECK(write(pfd[1], output, sizeof output) == (ssize_t)sizeof output);
  close(pfd[1]);

  FILE *rfp = fdopen(pfd[0], "r");
  CHECK(rfp != NULL);
  AsyncSession *s = make_async_session(rfp, 8);
  CHECK(s != NULL);

  async_reader((void *)s);

  CHECK(s->count == 1);
  CHECK(strcmp(cands_at(s, 0), "valid") == 0);
  uint64_t error = atomic_load_explicit(
      &s->producer_error, memory_order_acquire);
  CHECK(async_unpack_producer_error_kind(error) ==
        AsyncProducerErrorInvalidData);
  CHECK(atomic_load_explicit(&s->producer_state,
                             memory_order_acquire) == AsyncProducerFailed);
  CHECK(atomic_load_explicit(&s->reader_done, memory_order_acquire));
  free_async_session(s);
}

/* =====================================================================
 * Chunked candidate storage — index split formula and accessor
 * ===================================================================== */

static void test_cands_top_index_split(void) {
  /* Verify hi = i >> SHIFT and lo = i & MASK match the documented
     "i = hi * BLOCK_SIZE + lo" decomposition. */
  size_t cases[][3] = {
    /* { i, expected_hi, expected_lo } */
    {                           0, 0,                          0 },
    {                           1, 0,                          1 },
    { CANDS_BLOCK_SIZE       - 1, 0, CANDS_BLOCK_SIZE       - 1 },
    { CANDS_BLOCK_SIZE          , 1,                          0 },
    { CANDS_BLOCK_SIZE       + 5, 1,                          5 },
    { CANDS_BLOCK_SIZE * 2      , 2,                          0 },
    { CANDS_BLOCK_SIZE * 2   + 7, 2,                          7 },
    { CANDS_BLOCK_SIZE * 100    , 100,                        0 },
  };
  for (size_t k = 0; k < sizeof cases / sizeof *cases; k++) {
    size_t i = cases[k][0];
    CHECK((i >> CANDS_BLOCK_SHIFT) == cases[k][1]);
    CHECK((i & CANDS_BLOCK_MASK)   == cases[k][2]);
    /* Inverse: reconstruct i from (hi, lo). */
    CHECK((cases[k][1] << CANDS_BLOCK_SHIFT) + cases[k][2] == i);
  }
}

static void test_cands_top_accessor_reads_block_pointer(void) {
  /* Manually populate a single slot via the accessor formula and
     verify the read path returns the same pointer. */
  AsyncSession *s = calloc(1, sizeof *s);
  CHECK(s != NULL);
  pthread_mutex_init(&s->mu, NULL);

  /* Allocate block 3 and write a sentinel pointer at slot 42. */
  size_t hi = 3, lo = 42;
  s->cands_top[hi] = calloc(CANDS_BLOCK_SIZE, sizeof *s->cands_top[hi]);
  CHECK(s->cands_top[hi] != NULL);
  char *sentinel = "sentinel";
  s->cands_top[hi][lo] = sentinel;

  /* Read it back via the accessor formula at the equivalent global index. */
  size_t i = (hi << CANDS_BLOCK_SHIFT) + lo;
  CHECK(s->cands_top[i >> CANDS_BLOCK_SHIFT][i & CANDS_BLOCK_MASK] == sentinel);

  free(s->cands_top[hi]);
  pthread_mutex_destroy(&s->mu);
  free(s);
}

/* =====================================================================
 * Result cache — phase 1: exact-match lookup, LRU eviction, MRU touch
 * ===================================================================== */

static ScoredStr make_top(const char *str, int score) {
  ScoredStr s = {0};
  s.str   = (char *)str;   /* not freed by the cache (cache strdups internally) */
  s.score = score;
  return s;
}

static void test_cache_lookup_miss_on_empty(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr *out_top = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "foo", &out_top, &out_count,
                           &out_sidx, &out_gen) == false);
  CHECK(out_top == NULL);
  cache_free(&c);
}

static void test_cache_insert_then_lookup_hit(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[2] = { make_top("alpha", 42), make_top("beta", 17) };

  cache_insert(&c, "fo", 1000, CaseSmart, true, top, 2, NULL, 0);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "fo", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_count == 2);
  CHECK(out_gen == 1000);
  CHECK(out != NULL);
  CHECK(out[0].score == 42);
  CHECK(strcmp(out[0].str, "alpha") == 0);
  CHECK(out[1].score == 17);
  CHECK(strcmp(out[1].str, "beta") == 0);
  CHECK(out_sidx == NULL);   /* no matched_idx supplied */
  free(out);
  cache_free(&c);
}

static void test_cache_lookup_miss_distinct_query(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[1] = { make_top("alpha", 42) };
  cache_insert(&c, "fo", 100, CaseSmart, true, top, 1, NULL, 0);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "bar", &out, &out_count, &out_sidx, &out_gen) == false);
  CHECK(out == NULL);
  cache_free(&c);
}

static void test_cache_insert_updates_in_place(void) {
  /* Re-inserting the same query overwrites the existing entry rather
     than creating a duplicate.  Verify count stays at 1 and the new
     data wins. */
  Cache c;
  cache_init(&c, 20);
  ScoredStr v1[1] = { make_top("alpha", 10) };
  ScoredStr v2[2] = { make_top("alpha", 99), make_top("beta", 50) };

  cache_insert(&c, "fo", 100, CaseSmart, true, v1, 1, NULL, 0);
  cache_insert(&c, "fo", 200, CaseSmart, true, v2, 2, NULL, 0);
  CHECK(c.count == 1);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "fo", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_count == 2);
  CHECK(out_gen == 200);
  CHECK(out[0].score == 99);
  CHECK(out[1].score == 50);
  free(out);
  cache_free(&c);
}

static void test_cache_lru_eviction_at_capacity(void) {
  /* Fill the cache, insert one more, verify the oldest entry is gone
     and all others remain. */
  const size_t MAX = 8;
  Cache c;
  cache_init(&c, MAX);
  ScoredStr one[1] = { make_top("x", 1) };

  char qbuf[16];
  for (size_t i = 0; i < MAX; i++) {
    snprintf(qbuf, sizeof qbuf, "q%zu", i);
    cache_insert(&c, qbuf, (size_t)i, CaseSmart, true, one, 1, NULL, 0);
  }
  CHECK(c.count == MAX);

  /* Insert one more — should evict q0 (the LRU tail). */
  cache_insert(&c, "extra", 999, CaseSmart, true, one, 1, NULL, 0);
  CHECK(c.count == MAX);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;

  /* q0 is gone. */
  CHECK(cache_lookup_exact(&c, "q0", &out, &out_count, &out_sidx, &out_gen) == false);

  /* q1 .. q(MAX-1) are still present. */
  for (size_t i = 1; i < MAX; i++) {
    snprintf(qbuf, sizeof qbuf, "q%zu", i);
    out = NULL; out_sidx = NULL; out_count = 0; out_gen = 0;
    CHECK(cache_lookup_exact(&c, qbuf, &out, &out_count, &out_sidx, &out_gen) == true);
    free(out);
  }

  /* And the freshly inserted "extra" is present. */
  out = NULL; out_sidx = NULL; out_count = 0; out_gen = 0;
  CHECK(cache_lookup_exact(&c, "extra", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 999);
  free(out);

  cache_free(&c);
}

static void test_cache_touch_on_hit(void) {
  /* Fill the cache; touch q0 (the oldest) so it becomes MRU; insert
     one more; verify q0 survived and q1 (now the LRU) was evicted. */
  const size_t MAX = 4;
  Cache c;
  cache_init(&c, MAX);
  ScoredStr one[1] = { make_top("x", 1) };

  char qbuf[16];
  for (size_t i = 0; i < MAX; i++) {
    snprintf(qbuf, sizeof qbuf, "q%zu", i);
    cache_insert(&c, qbuf, (size_t)i, CaseSmart, true, one, 1, NULL, 0);
  }

  /* Touch q0 — moves it to head (MRU). */
  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "q0", &out, &out_count, &out_sidx, &out_gen) == true);
  free(out);

  /* Now the LRU tail is q1.  Insert one more; q1 should be evicted. */
  cache_insert(&c, "extra", 999, CaseSmart, true, one, 1, NULL, 0);

  out = NULL; out_sidx = NULL; out_count = 0; out_gen = 0;
  CHECK(cache_lookup_exact(&c, "q0", &out, &out_count, &out_sidx, &out_gen) == true);
  free(out);

  out = NULL; out_sidx = NULL; out_count = 0; out_gen = 0;
  CHECK(cache_lookup_exact(&c, "q1", &out, &out_count, &out_sidx, &out_gen) == false);

  cache_free(&c);
}

static void test_cache_insert_zero_count(void) {
  /* Empty top[] is a legitimate "no matches" cache entry; verify it
     stores and looks up cleanly. */
  Cache c;
  cache_init(&c, 20);
  cache_insert(&c, "nothing", 500, CaseSmart, true, NULL, 0, NULL, 0);

  ScoredStr *out = (ScoredStr *)0xdeadbeef;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 99, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "nothing", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out == NULL);
  CHECK(out_count == 0);
  CHECK(out_gen == 500);
  CHECK(out_sidx != NULL);
  CHECK(out_sidx && out_sidx->count == 0);
  shared_idx_release(out_sidx);
  cache_free(&c);
}

static void test_cache_evicts_to_byte_budget(void) {
  uint32_t idx[32];
  for (size_t i = 0; i < 32; i++) idx[i] = (uint32_t)i;
  ScoredStr top = make_top("candidate", 7);

  Cache probe;
  cache_init(&probe, 10);
  cache_insert_for_request(&probe, "aa", 32, CaseSmart, true, false,
                           &top, 1, 32, idx, 32);
  size_t one_entry_bytes = probe.used_bytes;
  CHECK(one_entry_bytes > 0);
  cache_free(&probe);

  Cache c;
  cache_init_limits(&c, 10, one_entry_bytes);
  cache_insert_for_request(&c, "aa", 32, CaseSmart, true, false,
                           &top, 1, 32, idx, 32);
  cache_insert_for_request(&c, "bb", 32, CaseSmart, true, false,
                           &top, 1, 32, idx, 32);
  CHECK(c.count == 1);
  CHECK(c.used_bytes <= c.max_bytes);
  CHECK(c.evictions == 1);
  cache_free(&c);
}

static void test_cache_skips_single_oversize_entry(void) {
  uint32_t idx[32];
  for (size_t i = 0; i < 32; i++) idx[i] = (uint32_t)i;
  Cache c;
  cache_init_limits(&c, 10, 64);
  cache_insert_for_request(&c, "large", 32, CaseSmart, true, false,
                           NULL, 0, 32, idx, 32);
  CHECK(c.count == 0);
  CHECK(c.used_bytes == 0);
  cache_free(&c);
}

static void test_cache_disabled_skips_insert_work(void) {
  Cache c;
  cache_init_limits(&c, 40, 0);
  ScoredStr top = {.str = "alpha", .score = 10, .idx = 0};
  uint32_t membership = 0;
  cache_insert_for_request(&c, "a", 1, CaseSmart, true, false,
                           &top, 1, 1, &membership, 1);
  size_t entries = 99, bytes = 99;
  uint64_t evictions = 99;
  cache_stats(&c, &entries, &bytes, &evictions);
  CHECK(entries == 0);
  CHECK(bytes == 0);
  CHECK(evictions == 0);
  cache_free(&c);
}

static void test_cache_pool_gen_distinguishes_stale(void) {
  /* The cache itself doesn't decide fresh-vs-stale — that's the dispatch
     layer's job — but it must faithfully report pool_gen so the dispatch
     can compare it against the current pool size. */
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[1] = { make_top("alpha", 1) };
  cache_insert(&c, "fo", 100, CaseSmart, true, top, 1, NULL, 0);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "fo", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 100);
  free(out);

  /* Re-insert at a new pool_gen; lookup should reflect the latest. */
  cache_insert(&c, "fo", 5000, CaseSmart, true, top, 1, NULL, 0);
  out = NULL; out_count = 0; out_gen = 0;
  CHECK(cache_lookup_exact(&c, "fo", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 5000);
  free(out);
  cache_free(&c);
}

static void test_cache_exact_separates_case_and_fuzzy_modes(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr ignore_top[1] = { make_top("ignore", 30) };
  ScoredStr respect_top[1] = { make_top("respect", 20) };
  ScoredStr exact_top[1] = { make_top("exact", 10) };

  cache_insert_for_request(&c, "foo", 10, CaseIgnore, true, false,
                           ignore_top, 1, 1, NULL, 0);
  cache_insert_for_request(&c, "foo", 10, CaseRespect, true, false,
                           respect_top, 1, 1, NULL, 0);
  cache_insert_for_request(&c, "foo", 10, CaseIgnore, false, false,
                           exact_top, 1, 1, NULL, 0);
  CHECK(c.count == 3);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0, matched_count = 0;
  bool covered = false;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseIgnore, true, false, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(out_count == 1);
  CHECK(strcmp(out[0].str, "ignore") == 0);
  CHECK(covered);
  free(out);

  out = NULL;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseRespect, true, false, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(strcmp(out[0].str, "respect") == 0);
  free(out);

  out = NULL;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseIgnore, false, false, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(strcmp(out[0].str, "exact") == 0);
  free(out);
  cache_free(&c);
}

static void test_cache_exact_requires_sufficient_result_capacity(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr top_one[1] = { make_top("one", 30) };
  uint32_t matches[3] = { 0, 1, 2 };
  cache_insert_for_request(&c, "foo", 10, CaseSmart, true, false,
                           top_one, 1, 3, matches, 3);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0, matched_count = 0;
  bool covered = false;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseSmart, true, false, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(covered);
  CHECK(matched_count == 3);
  free(out);
  shared_idx_release(out_sidx);

  out = NULL; out_sidx = NULL; covered = true;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseSmart, true, false, 3,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(!covered);
  free(out);
  shared_idx_release(out_sidx);

  out = NULL; out_sidx = NULL; covered = true;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseSmart, true, false, 0,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(!covered);
  free(out);
  shared_idx_release(out_sidx);
  cache_free(&c);
}

static void test_cache_exact_caps_result_to_requested_limit(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[3] = {
      make_top("one", 30), make_top("two", 20), make_top("three", 10)};
  uint32_t matches[3] = {0, 1, 2};
  cache_insert_for_request(&c, "foo", 3, CaseSmart, true, false,
                           top, 3, 3, matches, 3);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0, matched_count = 0;
  bool covered = false;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseSmart, true, false, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(covered);
  CHECK(out_count == 1);
  CHECK(strcmp(out[0].str, "one") == 0);
  CHECK(matched_count == 3);
  free(out);
  shared_idx_release(out_sidx);
  cache_free(&c);
}

static void test_cache_exact_filter_only_requires_same_emit_window(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[3] = {
      make_top("best-late", 30),
      make_top("middle", 20),
      make_top("first-weak", 10),
  };
  uint32_t matches[3] = {0, 1, 2};
  cache_insert_for_request(&c, "a", 3, CaseSmart, true, true,
                           top, 3, 3, matches, 3);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0, matched_count = 0;
  bool covered = true;
  CHECK(cache_lookup_exact_for_request(
      &c, "a", CaseSmart, true, true, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(!covered);
  CHECK(out_count == 1);
  CHECK(out_sidx != NULL);
  free(out);
  shared_idx_release(out_sidx);

  out = NULL; out_sidx = NULL; covered = false;
  CHECK(cache_lookup_exact_for_request(
      &c, "a", CaseSmart, true, true, 3,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(covered);
  CHECK(out_count == 3);
  free(out);
  shared_idx_release(out_sidx);
  cache_free(&c);
}

static void test_cache_exact_reuses_membership_across_filter_only_mode(void) {
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[1] = { make_top("one", 30) };
  uint32_t matches[1] = { 0 };
  cache_insert_for_request(&c, "foo", 10, CaseSmart, true, true,
                           top, 1, 1, matches, 1);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0, matched_count = 0;
  bool covered = true;
  CHECK(cache_lookup_exact_for_request(
      &c, "foo", CaseSmart, true, false, 1,
      &out, &out_count, &out_sidx, &out_gen,
      &matched_count, &covered));
  CHECK(!covered);
  CHECK(out_sidx != NULL);
  CHECK(out_sidx->count == 1);
  free(out);
  shared_idx_release(out_sidx);
  cache_free(&c);
}

/* =====================================================================
 * Result cache — phase 2: term-set subsumption + prefix lookup
 * ===================================================================== */

static void test_subsumes_pattern_extending_term_via_byte_prefix(void) {
  /* "fo" → "foo": same single-term query getting longer.  Term-set rule
     alone says NO (terms "fo" and "foo" aren't equivalent), but
     cache_lookup_prefix uses byte-prefix OR term-set, so this still
     captures via the byte-prefix path.  Verify the byte-prefix subsumes()
     directly. */
  CHECK(subsumes("fo", "foo") == true);
}

static void test_byte_prefix_rejects_operator_source_terms(void) {
  CHECK(subsumes("!foo", "!foobar") == false);
  CHECK(subsumes("foo$", "foo$bar") == false);
  CHECK(subsumes("^foo", "^foobar") == false);
  CHECK(subsumes("'foo", "'foobar") == false);
  CHECK(subsumes("foo bar", "foo bar baz") == false);
}

static void test_subsumes_pattern_adding_term_at_end(void) {
  /* "fo" → "fo bar": both rules agree.  Verify term-set path. */
  fzf_pattern_t *p1 = parse_query_for_cache("fo", CaseSmart, true);
  fzf_pattern_t *p2 = parse_query_for_cache("fo bar", CaseSmart, true);
  CHECK(p1 && p2);
  CHECK(subsumes_pattern(p1, p2) == true);
  CHECK(subsumes_pattern(p2, p1) == false);
  fzf_free_pattern(p1);
  fzf_free_pattern(p2);
}

static void test_subsumes_pattern_adding_term_at_start(void) {
  /* "fo" → "x fo": v2-only case.  Byte-prefix says NO (fo not prefix of
     x fo), term-set says YES (fo's terms ⊆ x fo's terms). */
  fzf_pattern_t *p1 = parse_query_for_cache("fo", CaseSmart, true);
  fzf_pattern_t *p2 = parse_query_for_cache("x fo", CaseSmart, true);
  CHECK(p1 && p2);
  CHECK(subsumes("fo", "x fo") == false);            /* v1 misses */
  CHECK(subsumes_pattern(p1, p2) == true);           /* v2 catches */
  CHECK(subsumes_pattern(p2, p1) == false);
  fzf_free_pattern(p1);
  fzf_free_pattern(p2);
}

static void test_subsumes_pattern_term_reorder(void) {
  /* "foo bar" and "bar foo" are semantically equivalent in fzf — same
     term set, different textual order.  Term-set rule sees mutual
     subsumption; byte-prefix rule sees neither. */
  fzf_pattern_t *p1 = parse_query_for_cache("foo bar", CaseSmart, true);
  fzf_pattern_t *p2 = parse_query_for_cache("bar foo", CaseSmart, true);
  CHECK(p1 && p2);
  CHECK(subsumes("foo bar", "bar foo") == false);
  CHECK(subsumes("bar foo", "foo bar") == false);
  CHECK(subsumes_pattern(p1, p2) == true);
  CHECK(subsumes_pattern(p2, p1) == true);
  fzf_free_pattern(p1);
  fzf_free_pattern(p2);
}

static void test_subsumes_pattern_negation_at_start(void) {
  /* "fo" → "!x fo": adding a negation term in non-prefix position.
     Term-set rule catches it; byte-prefix doesn't. */
  fzf_pattern_t *p1 = parse_query_for_cache("fo", CaseSmart, true);
  fzf_pattern_t *p2 = parse_query_for_cache("!x fo", CaseSmart, true);
  CHECK(p1 && p2);
  CHECK(subsumes_pattern(p1, p2) == true);
  fzf_free_pattern(p1);
  fzf_free_pattern(p2);
}

static void test_subsumes_pattern_or_query_rejected(void) {
  /* "fo | bar" parses as ONE term-set with TWO terms (within a set =
     OR; across sets = AND).  subsumes_pattern rejects any term-set with
     >1 term — it can never serve as a refinement source. */
  fzf_pattern_t *p1 = parse_query_for_cache("fo", CaseSmart, true);
  fzf_pattern_t *p2 = parse_query_for_cache("fo | bar", CaseSmart, true);
  CHECK(p1 && p2);
  CHECK(p1->size == 1 && p1->ptr[0]->size == 1);  /* "fo": 1 set, 1 term */
  CHECK(p2->size == 1 && p2->ptr[0]->size == 2);  /* "fo|bar": 1 set, 2 terms */
  CHECK(subsumes_pattern(p1, p2) == false);
  CHECK(subsumes_pattern(p2, p1) == false);
  fzf_free_pattern(p1);
  fzf_free_pattern(p2);
}

static void test_subsumes_pattern_distinct_terms(void) {
  /* "foo" and "bar" share no terms; neither subsumes the other. */
  fzf_pattern_t *p1 = parse_query_for_cache("foo", CaseSmart, true);
  fzf_pattern_t *p2 = parse_query_for_cache("bar", CaseSmart, true);
  CHECK(p1 && p2);
  CHECK(subsumes_pattern(p1, p2) == false);
  CHECK(subsumes_pattern(p2, p1) == false);
  fzf_free_pattern(p1);
  fzf_free_pattern(p2);
}

/* Helper: insert a cache entry that has a non-NULL m_idx (so it's eligible
   as a prefix-refinement source) using a single dummy match index.  Tests
   the lookup logic without caring about the actual indices. */
static void cache_insert_eligible(Cache *c, const char *query, size_t pool_gen) {
  uint32_t idx[1] = { 0 };
  cache_insert(c, query, pool_gen, CaseSmart, true, NULL, 0, idx, 1);
}

static void test_cache_lookup_prefix_v2_finds_term_subset(void) {
  /* Cache has "fo".  New query "x fo" should hit via term-set rule. */
  Cache c;
  cache_init(&c, 20);
  cache_insert_eligible(&c, "fo", 100);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_prefix(&c, "x fo", CaseSmart, true, &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 100);
  shared_idx_release(out_sidx);
  free(out);
  cache_free(&c);
}

static void test_cache_lookup_prefix_rejects_inverse_extension(void) {
  Cache c;
  cache_init(&c, 20);
  cache_insert_eligible(&c, "!foo", 100);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(!cache_lookup_prefix(&c, "!foobar", CaseSmart, true,
                             &out, &out_count, &out_sidx, &out_gen));
  CHECK(out == NULL);
  CHECK(out_sidx == NULL);
  cache_free(&c);
}

static void test_cache_lookup_prefix_v2_finds_reordered(void) {
  /* Cache has "foo bar".  New query "bar foo" should hit via term-set
     mutual subsumption.  We exclude exact-match entries from prefix
     lookup, but "bar foo" != "foo bar" textually so it counts as
     non-exact and the term-set rule picks it up. */
  Cache c;
  cache_init(&c, 20);
  cache_insert_eligible(&c, "foo bar", 100);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_prefix(&c, "bar foo", CaseSmart, true, &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 100);
  shared_idx_release(out_sidx);
  free(out);
  cache_free(&c);
}

static void test_cache_lookup_prefix_picks_most_terms(void) {
  /* Cache has "fo" (1 term) and "fo bar" (2 terms).  New query
     "fo bar baz" subsumes both.  cache_lookup_prefix should prefer the
     most-restricted entry — "fo bar" with 2 terms — over "fo". */
  Cache c;
  cache_init(&c, 20);
  cache_insert_eligible(&c, "fo",     100);
  cache_insert_eligible(&c, "fo bar", 200);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_prefix(&c, "fo bar baz", CaseSmart, true, &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 200);   /* "fo bar" entry wins */
  shared_idx_release(out_sidx);
  free(out);
  cache_free(&c);
}

static void test_cache_lookup_prefix_skips_or_in_query(void) {
  /* If the new query contains '|', prefix lookup short-circuits to false
     (we never refine into an OR query). */
  Cache c;
  cache_init(&c, 20);
  cache_insert_eligible(&c, "fo", 100);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_prefix(&c, "fo | bar", CaseSmart, true, &out, &out_count, &out_sidx, &out_gen) == false);
  cache_free(&c);
}

static void test_cache_lookup_prefix_skips_exact_match(void) {
  /* Even if an entry's parsed pattern equals the new query's, we exclude
     it from prefix lookup (that's what cache_lookup_exact is for). */
  Cache c;
  cache_init(&c, 20);
  cache_insert_eligible(&c, "fo bar", 100);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_prefix(&c, "fo bar", CaseSmart, true, &out, &out_count, &out_sidx, &out_gen) == false);
  cache_free(&c);
}

static void test_batch_cache_sparse_bitmap_and_selectivity_cutoff(void) {
  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *query = batch_cache_acquire_query(
      &cache, "foo", CaseSmart, true);
  CHECK(query != NULL);

  ScoredStr sparse[3] = {
    {.idx = 2 * BATCH_SIZE + 0},
    {.idx = 2 * BATCH_SIZE + 17},
    {.idx = 2 * BATCH_SIZE + 2047},
  };
  batch_cache_insert(&cache, query, 2, sparse, 3);
  uint16_t local[BATCH_SIZE];
  size_t count = 0;
  CHECK(batch_cache_copy_members(&cache, query, 2, local, &count));
  CHECK(count == 3);
  CHECK(local[0] == 0 && local[1] == 17 && local[2] == 2047);

  ScoredStr bitmap[200];
  for (size_t i = 0; i < 200; i++)
    bitmap[i].idx = (uint32_t)(3 * BATCH_SIZE + i * 3);
  batch_cache_insert(&cache, query, 3, bitmap, 200);
  count = 0;
  CHECK(batch_cache_copy_members(&cache, query, 3, local, &count));
  CHECK(count == 200);
  for (size_t i = 0; i < count; i++) CHECK(local[i] == i * 3);

  size_t stats_entries = 0, stats_bytes = 0;
  uint64_t stats_hits = 0, stats_misses = 0, stats_evictions = 0;
  batch_cache_stats(&cache, &stats_entries, &stats_bytes,
                    &stats_hits, &stats_misses, &stats_evictions);
  size_t traversed_entries = 0;
  for (BatchCacheEntry *entry = cache.head; entry;
       entry = entry->lru_next)
    traversed_entries++;
  CHECK(stats_entries == 2);
  CHECK(stats_entries == traversed_entries);
  CHECK(stats_entries == cache.entry_count);

  ScoredStr *dense = calloc(BATCH_SIZE / 2 + 1, sizeof *dense);
  CHECK(dense != NULL);
  if (dense) {
    for (size_t i = 0; i < BATCH_SIZE / 2 + 1; i++)
      dense[i].idx = (uint32_t)(4 * BATCH_SIZE + i);
    batch_cache_insert(&cache, query, 4, dense, BATCH_SIZE / 2 + 1);
    count = 0;
    CHECK(!batch_cache_copy_members(&cache, query, 4, local, &count));
    free(dense);
  }

  batch_cache_release_query(&cache, query);
  batch_cache_free(&cache);
}

static void test_batch_cache_selects_only_safe_query_ancestors(void) {
  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *foo = batch_cache_acquire_query(
      &cache, "foo", CaseSmart, true);
  ScoredStr match = {.idx = 1};
  batch_cache_insert(&cache, foo, 0, &match, 1);
  batch_cache_release_query(&cache, foo);

  BatchQuery *source = batch_cache_select_source(
      &cache, "x foo", CaseSmart, true);
  CHECK(source != NULL);
  CHECK(source && strcmp(source->query, "foo") == 0);
  batch_cache_release_query(&cache, source);

  source = batch_cache_select_source(&cache, "fo", CaseSmart, true);
  CHECK(source == NULL);
  source = batch_cache_select_source(&cache, "x foo", CaseRespect, true);
  CHECK(source == NULL);
  source = batch_cache_select_source(&cache, "foo | bar", CaseSmart, true);
  CHECK(source == NULL);

  BatchQuery *inverse = batch_cache_acquire_query(
      &cache, "!foo", CaseSmart, true);
  batch_cache_insert(&cache, inverse, 0, &match, 1);
  batch_cache_release_query(&cache, inverse);
  source = batch_cache_select_source(
      &cache, "!foobar", CaseSmart, true);
  CHECK(source == NULL);

  batch_cache_free(&cache);
}

static void test_batch_cache_evicts_to_byte_budget(void) {
  char mutable_query[] = "foo";
  fzf_pattern_t *parsed = fzf_parse_pattern(
      CaseSmart, false, mutable_query, true);
  CHECK(parsed != NULL);
  size_t parsed_bytes = cache_pattern_bytes(parsed);
  CHECK(parsed_bytes > 0 && parsed_bytes < SIZE_MAX);
  if (parsed) fzf_free_pattern(parsed);

  size_t query_bytes = sizeof(BatchQuery) + strlen("foo") + 1 +
                       parsed_bytes;
  size_t budget = query_bytes +
                  sizeof(BatchCacheEntry) + sizeof(uint16_t);
  BatchCache cache;
  batch_cache_init(&cache, budget);
  BatchQuery *query = batch_cache_acquire_query(
      &cache, "foo", CaseSmart, true);
  CHECK(query != NULL);
  CHECK(query && query->bytes == query_bytes);
  CHECK(cache.used_bytes == query_bytes);
  if (!query) {
    batch_cache_free(&cache);
    return;
  }
  ScoredStr match = {.idx = 0};
  batch_cache_insert(&cache, query, 0, &match, 1);
  match.idx = BATCH_SIZE;
  batch_cache_insert(&cache, query, 1, &match, 1);

  uint16_t local[BATCH_SIZE];
  size_t count = 0;
  CHECK(!batch_cache_copy_members(&cache, query, 0, local, &count));
  CHECK(batch_cache_copy_members(&cache, query, 1, local, &count));
  CHECK(count == 1 && local[0] == 0);
  CHECK(cache.evictions == 1);
  CHECK(cache.entry_count == 1);
  CHECK(cache.used_bytes <= cache.max_bytes);

  batch_cache_release_query(&cache, query);
  batch_cache_free(&cache);

  BatchCache too_small;
  batch_cache_init(&too_small, query_bytes - 1);
  query = batch_cache_acquire_query(
      &too_small, "foo", CaseSmart, true);
  CHECK(query == NULL);
  CHECK(too_small.used_bytes == 0);
  batch_cache_free(&too_small);
}

static void test_completed_batch_evidence_survives_request_cancellation(void) {
  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *target = batch_cache_acquire_query(
      &cache, "a", CaseSmart, true);
  struct AsyncScoringBatch *batch = calloc(1, sizeof *batch);
  CHECK(target != NULL);
  CHECK(batch != NULL);
  if (!target || !batch) {
    free(batch);
    batch_cache_release_query(&cache, target);
    batch_cache_free(&cache);
    return;
  }

  batch->batch_id = 0;
  batch->cacheable = true;
  batch->len = BATCH_SIZE;
  for (size_t i = 0; i < BATCH_SIZE; i++) {
    batch->xs[i].str = i < 3 ? "alpha" : "zzz";
    batch->xs[i].idx = (uint32_t)i;
  }
  char pattern_text[] = "a";
  fzf_pattern_t *pattern = fzf_parse_pattern(
      CaseSmart, false, pattern_text, true);
  _Atomic bool abort = false;
  _Atomic size_t progress = 0;
  struct AsyncScoringShared shared = {
    .pattern = pattern,
    .batches = batch,
    .remaining = 1,
    .stop = &abort,
    .progress_completed = &progress,
    .batch_cache = &cache,
    .target_query = target,
    .filter_only = false,
  };
  fzf_slab_t *slab = fzf_make_default_slab();
  async_score_batches(&shared, slab, 0);
  CHECK(batch->len == 3);
  CHECK(atomic_load_explicit(&progress, memory_order_relaxed) == BATCH_SIZE);

  /* No whole-request result is published.  The batch entry is already safe. */
  uint16_t local[BATCH_SIZE];
  size_t count = 0;
  CHECK(batch_cache_copy_members(&cache, target, 0, local, &count));
  CHECK(count == 3);
  CHECK(local[0] == 0 && local[1] == 1 && local[2] == 2);

  if (slab) fzf_free_slab(slab);
  if (pattern) fzf_free_pattern(pattern);
  free(batch);
  batch_cache_release_query(&cache, target);
  batch_cache_free(&cache);
}

static void test_mutable_partial_batch_does_not_enter_batch_cache(void) {
  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *target = batch_cache_acquire_query(
      &cache, "", CaseSmart, true);
  struct AsyncScoringBatch batch = {
    .len = 1,
    .batch_id = 0,
    .cacheable = false,
    .xs = {{.str = "alpha", .idx = 0}},
  };
  _Atomic bool abort = false;
  struct AsyncScoringShared shared = {
    .pattern = NULL,
    .batches = &batch,
    .remaining = 1,
    .stop = &abort,
    .batch_cache = &cache,
    .target_query = target,
    .filter_only = false,
  };
  fzf_slab_t *slab = fzf_make_default_slab();
  async_score_batches(&shared, slab, 0);
  uint16_t local[BATCH_SIZE];
  size_t count = 0;
  CHECK(!batch_cache_copy_members(&cache, target, 0, local, &count));

  if (slab) fzf_free_slab(slab);
  batch_cache_release_query(&cache, target);
  batch_cache_free(&cache);
}

/* =====================================================================
 * Interactive request identity
 * ===================================================================== */

static void test_async_request_state_precedence(void) {
  CHECK(async_request_state(0, 0, 0, 0, 0, 0) == AsyncRequestIdle);
  CHECK(async_request_state(4, 4, 4, 0, 3, 0) == AsyncRequestQueued);
  CHECK(async_request_state(4, 4, 0, 4, 3, 0) == AsyncRequestRunning);
  CHECK(async_request_state(4, 4, 4, 0, 4, 0) == AsyncRequestQueued);
  CHECK(async_request_state(4, 4, 0, 4, 4, 0) == AsyncRequestRunning);
  CHECK(async_request_state(4, 5, 5, 0, 4, 0) == AsyncRequestComplete);
  CHECK(async_request_state(4, 4, 0, 0, 3, 4) == AsyncRequestFailed);
  CHECK(async_request_state(4, 4, 0, 0, 4, 4) == AsyncRequestFailed);
  CHECK(async_request_state(4, 5, 5, 0, 3, 0) == AsyncRequestSuperseded);
  CHECK(async_request_state(6, 5, 0, 0, 5, 0) == AsyncRequestUnknown);
}

static void test_async_batch_window_bounds_preparation_memory(void) {
  const size_t candidates = 60000000;
  size_t batches = (candidates + BATCH_SIZE - 1) / BATCH_SIZE;
  size_t window = MIN(batches, (size_t)ASYNC_BATCH_WINDOW);
  CHECK(window == ASYNC_BATCH_WINDOW);
  CHECK(window * sizeof(struct AsyncScoringBatch) < 3 * 1024 * 1024);
  CHECK(batches * sizeof(struct AsyncScoringBatch) > 900 * 1024 * 1024);

  ScoredStr *values = NULL;
  size_t capacity = 0;
  CHECK(async_scored_reserve(&values, &capacity, 1));
  CHECK(values != NULL);
  CHECK(capacity >= 1);
  if (values) values[0] = (ScoredStr){.str = "kept", .score = 7, .idx = 3};
  CHECK(async_scored_reserve(&values, &capacity, 10000));
  CHECK(capacity >= 10000);
  CHECK(values && values[0].score == 7 && values[0].idx == 3);

  ScoredStr *before = values;
  size_t capacity_before = capacity;
  CHECK(!async_scored_reserve(
      &values, &capacity, SIZE_MAX / sizeof *values + 1));
  CHECK(values == before);
  CHECK(capacity == capacity_before);
  free(values);
}

struct PoolObservationRace {
  AsyncSession *session;
  _Atomic bool start;
  _Atomic bool attempting_append;
};

static struct PoolObservationRace pool_observation_race;

static void *pool_observation_reader(void *unused) {
  (void)unused;
  while (!atomic_load_explicit(&pool_observation_race.start,
                               memory_order_acquire))
    sched_yield();

  /* Announce before locking: the observer's test hook still owns MU here, so
     this update cannot publish until both COUNT and READER_DONE are sampled. */
  atomic_store_explicit(&pool_observation_race.attempting_append, true,
                        memory_order_release);
  AsyncSession *s = pool_observation_race.session;
  pthread_mutex_lock(&s->mu);
  s->count = 2;
  pthread_mutex_unlock(&s->mu);
  atomic_store_explicit(&s->reader_done, true, memory_order_release);
  return NULL;
}

static void pool_observation_release_reader(AsyncSession *s) {
  CHECK(s == pool_observation_race.session);
  atomic_store_explicit(&pool_observation_race.start, true,
                        memory_order_release);
  while (!atomic_load_explicit(&pool_observation_race.attempting_append,
                               memory_order_acquire))
    sched_yield();
}

static void test_async_pool_observation_is_terminally_consistent(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.mu, NULL);
  s.count = 1;
  atomic_store_explicit(&s.reader_done, false, memory_order_relaxed);
  pool_observation_race = (struct PoolObservationRace){
      .session = &s,
      .start = false,
      .attempting_append = false,
  };

  pthread_t reader;
  CHECK(pthread_create(&reader, NULL, pool_observation_reader, NULL) == 0);
  async_test_pool_observation_hook = pool_observation_release_reader;
  AsyncPoolObservation before = async_observe_pool(&s);
  async_test_pool_observation_hook = NULL;

  /* The simulated reader was definitely trying to append between the two
     field reads.  MU keeps the observation conservative: old/false is valid;
     old/true is the bug that made an incomplete result look final. */
  CHECK(before.count == 1);
  CHECK(!before.reader_done);
  pthread_join(reader, NULL);

  AsyncPoolObservation after = async_observe_pool(&s);
  CHECK(after.count == 2);
  CHECK(after.reader_done);
  pthread_mutex_destroy(&s.mu);
}

static void test_matcher_failure_retains_last_completed_result(void) {
  AsyncSession *s = calloc(1, sizeof *s);
  CHECK(s != NULL);
  if (!s) return;
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  s->score_current_id = 8;
  s->score_current_filter = strdup("new");
  s->score_result_id = 7;
  s->score_results = malloc(sizeof *s->score_results);
  CHECK(s->score_current_filter != NULL);
  CHECK(s->score_results != NULL);
  if (s->score_results) {
    s->score_results[0] = (ScoredStr){.str = "prior", .score = 10, .idx = 0};
    s->score_count = 1;
  }
  s->score_snapshot_generation = 4;

  async_publish_score_failure(s, 8, "matcher failed");

  CHECK(s->score_current_id == 0);
  CHECK(s->score_current_filter == NULL);
  CHECK(s->score_error_id == 8);
  CHECK(s->score_error && strcmp(s->score_error, "matcher failed") == 0);
  CHECK(s->score_snapshot_generation == 5);
  CHECK(s->score_result_id == 7);
  CHECK(s->score_count == 1);
  CHECK(s->score_results && strcmp(s->score_results[0].str, "prior") == 0);

  free(s->score_results);
  free(s->score_error);
  pthread_mutex_destroy(&s->score_res_mu);
  pthread_mutex_destroy(&s->score_req_mu);
  free(s);
}

static void test_async_request_identity_includes_matching_options(void) {
  CHECK(async_request_matches("foo", 10, CaseSmart, true, 2, false,
                              "foo", 10, CaseSmart, true, 2, false));
  CHECK(!async_request_matches("foo", 10, CaseSmart, true, 2, false,
                               "bar", 10, CaseSmart, true, 2, false));
  CHECK(!async_request_matches("foo", 10, CaseSmart, true, 2, false,
                               "foo", 20, CaseSmart, true, 2, false));
  CHECK(!async_request_matches("foo", 10, CaseSmart, true, 2, false,
                               "foo", 10, CaseRespect, true, 2, false));
  CHECK(!async_request_matches("foo", 10, CaseSmart, true, 2, false,
                               "foo", 10, CaseSmart, false, 2, false));
  CHECK(!async_request_matches("foo", 10, CaseSmart, true, 2, false,
                               "foo", 10, CaseSmart, true, 3, false));
  CHECK(!async_request_matches("foo", 10, CaseSmart, true, 2, false,
                               "foo", 10, CaseSmart, true, 2, true));
}

static void test_async_submit_oom_does_not_publish_request(void) {
  AsyncSession s = {0};
  pthread_mutex_init(&s.mu, NULL);
  pthread_mutex_init(&s.score_req_mu, NULL);
  pthread_mutex_init(&s.score_res_mu, NULL);
  cache_init_limits(&s.cache, 40, 64 * 1024 * 1024);

  char *filter = malloc(sizeof "needle");
  CHECK(filter != NULL);
  if (!filter) {
    cache_free(&s.cache);
    pthread_mutex_destroy(&s.score_res_mu);
    pthread_mutex_destroy(&s.score_req_mu);
    pthread_mutex_destroy(&s.mu);
    return;
  }
  memcpy(filter, "needle", sizeof "needle");

  /* The prefix-cache lookup retains a temporary query first.  Fault the
     following copy that would own SCORE_LATEST_FILTER. */
  atomic_store_explicit(&ctest_strdup_calls, 0, memory_order_relaxed);
  atomic_store_explicit(&ctest_strdup_fail_at, 2, memory_order_relaxed);
  uint64_t request_id = async_submit_request_resolved(
      &s, filter, strlen("needle"), 20, CaseSmart, true, 0, false);
  atomic_store_explicit(&ctest_strdup_fail_at, 0, memory_order_relaxed);

  CHECK(request_id == 0);
  CHECK(atomic_load_explicit(&ctest_strdup_calls,
                             memory_order_relaxed) == 2);
  CHECK(s.score_next_id == 0);
  CHECK(s.score_latest_id == 0);
  CHECK(s.score_latest_filter == NULL);
  CHECK(s.score_req_id == 0);
  CHECK(s.score_req_filter == NULL);
  CHECK(!atomic_load_explicit(&s.score_has_request, memory_order_acquire));

  cache_free(&s.cache);
  pthread_mutex_destroy(&s.score_res_mu);
  pthread_mutex_destroy(&s.score_req_mu);
  pthread_mutex_destroy(&s.mu);
}

static void test_async_running_request_reuse_requires_latest_slot(void) {
  AsyncSession s = {0};
  s.score_current_id = 7;
  s.score_latest_id = 7;
  s.score_current_filter = "foo";
  s.score_current_limit = 10;
  s.score_current_case_mode = CaseSmart;
  s.score_current_fuzzy = true;
  s.score_current_filter_only_length = 2;
  s.score_current_filter_only_logic_and = false;

  CHECK(async_current_request_reusable(
      &s, "foo", 10, CaseSmart, true, 2, false));

  /* A queued replacement means a later change back to the running query is
     a new request.  Reusing ID 7 here would allow queued ID 8 to win. */
  s.score_req_id = 8;
  s.score_latest_id = 8;
  CHECK(!async_current_request_reusable(
      &s, "foo", 10, CaseSmart, true, 2, false));

  s.score_req_id = 0;
  CHECK(!async_current_request_reusable(
      &s, "foo", 10, CaseSmart, true, 2, false));
}

static void test_sleep_ms(long milliseconds);
static double monotonic_ms(void);

struct TestPoolRunner {
  struct AsyncWorkerPool *pool;
  struct AsyncScoringShared *job;
  _Atomic bool returned;
  _Atomic unsigned *completion_sequence;
  _Atomic unsigned return_order;
};

static void *test_pool_runner(void *opaque) {
  struct TestPoolRunner *runner = opaque;
  async_worker_pool_run(runner->pool, runner->job);
  if (runner->completion_sequence)
    atomic_store_explicit(
        &runner->return_order,
        atomic_fetch_add_explicit(runner->completion_sequence, 1,
                                  memory_order_acq_rel) + 1,
        memory_order_release);
  atomic_store_explicit(&runner->returned, true, memory_order_release);
  return NULL;
}

static void test_pool_overlap_timeout(int signal_number) {
  (void)signal_number;
  static const char message[] =
      "fzf-native ctest: overlapping worker-pool jobs deadlocked\n";
  write(STDERR_FILENO, message, sizeof message - 1);
  _exit(124);
}

static void test_async_worker_pool_reuses_threads_across_rounds(void) {
  struct AsyncWorkerPool *pool = async_worker_pool_create(3);
  CHECK(pool != NULL);
  CHECK(pool->count > 0);
  unsigned worker_count = pool->count;
  pthread_t *worker_ids = malloc(worker_count * sizeof *worker_ids);
  CHECK(worker_ids != NULL);
  if (!worker_ids) {
    async_worker_pool_destroy(pool);
    return;
  }
  memcpy(worker_ids, pool->threads, worker_count * sizeof *worker_ids);

  _Atomic bool abort = false;
  _Atomic size_t progress_completed = 0;
  for (unsigned round = 0; round < 2; round++) {
    atomic_store_explicit(&progress_completed, 0, memory_order_relaxed);
    struct AsyncScoringBatch *batches = calloc(2, sizeof *batches);
    CHECK(batches != NULL);
    if (!batches) break;
    batches[0].len = 2;
    batches[0].xs[0].str = "alpha";
    batches[0].xs[1].str = "beta";
    batches[1].len = 1;
    batches[1].xs[0].str = "gamma";
    struct AsyncScoringShared shared = {
      .pattern = NULL,
      .batches = batches,
      .remaining = 2,
      .stop = &abort,
      .progress_completed = &progress_completed,
      .filter_only = false,
    };

    async_worker_pool_run(pool, &shared);

    CHECK(batches[0].len == 2);
    CHECK(batches[1].len == 1);
    CHECK(batches[0].xs[0].score == 1);
    CHECK(batches[0].xs[1].score == 1);
    CHECK(batches[1].xs[0].score == 1);
    CHECK(atomic_load_explicit(&progress_completed,
                               memory_order_relaxed) == 3);
    CHECK(pool->count == worker_count);
    for (unsigned i = 0; i < worker_count; i++)
      CHECK(pthread_equal(worker_ids[i], pool->threads[i]));
    free(batches);
  }
  CHECK(pool->epoch == 2);
  free(worker_ids);
  async_worker_pool_destroy(pool);
}

static void test_async_worker_pool_serializes_overlapping_sessions(void) {
  /* One worker plus a locked batch cache gives us a deterministic overlap:
     A has claimed its batch but cannot finish publishing it when B enters
     async_worker_pool_run.  B must wait without replacing A's descriptor. */
  void (*old_alarm_handler)(int) = signal(SIGALRM, test_pool_overlap_timeout);
  alarm(5);

  struct AsyncWorkerPool *pool = async_worker_pool_create(1);
  CHECK(pool != NULL);
  if (!pool) {
    alarm(0);
    signal(SIGALRM, old_alarm_handler);
    return;
  }

  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *target = batch_cache_acquire_query(
      &cache, "", CaseSmart, true);
  CHECK(target != NULL);
  if (!target) {
    async_worker_pool_destroy(pool);
    alarm(0);
    signal(SIGALRM, old_alarm_handler);
    return;
  }

  struct AsyncScoringBatch a_batch = {
    .len = 1,
    .cacheable = true,
    .xs = {{.str = "alpha", .idx = 0}},
  };
  struct AsyncScoringBatch b_batch = {
    .len = 1,
    .xs = {{.str = "beta", .idx = 0}},
  };
  _Atomic bool a_stop = false, b_stop = false;
  _Atomic size_t a_progress = 0, b_progress = 0;
  struct AsyncScoringShared a = {
    .pattern = NULL,
    .batches = &a_batch,
    .remaining = 1,
    .stop = &a_stop,
    .progress_completed = &a_progress,
    .batch_cache = &cache,
    .target_query = target,
  };
  struct AsyncScoringShared b = {
    .pattern = NULL,
    .batches = &b_batch,
    .remaining = 1,
    .stop = &b_stop,
    .progress_completed = &b_progress,
  };
  struct TestPoolRunner a_runner = {.pool = pool, .job = &a};
  struct TestPoolRunner b_runner = {.pool = pool, .job = &b};
  pthread_t a_thread, b_thread;

  pthread_mutex_lock(&cache.mu);
  CHECK(pthread_create(&a_thread, NULL, test_pool_runner, &a_runner) == 0);
  double deadline = monotonic_ms() + 1000.0;
  while (atomic_load_explicit(&a.remaining, memory_order_acquire) != 0 &&
         monotonic_ms() < deadline)
    test_sleep_ms(1);
  CHECK(atomic_load_explicit(&a.remaining, memory_order_acquire) == 0);

  CHECK(pthread_create(&b_thread, NULL, test_pool_runner, &b_runner) == 0);
  deadline = monotonic_ms() + 1000.0;
  size_t waiting = 0;
  uint64_t epoch = 0;
  struct AsyncScoringShared *published = NULL;
  struct AsyncScoringShared *queued = NULL;
  do {
    pthread_mutex_lock(&pool->mu);
    waiting = pool->waiting;
    epoch = pool->epoch;
    published = pool->job;
    queued = pool->wait_head ? pool->wait_head->job : NULL;
    pthread_mutex_unlock(&pool->mu);
    if (waiting == 1) break;
    test_sleep_ms(1);
  } while (monotonic_ms() < deadline);

  CHECK(waiting == 1);
  CHECK(epoch == 1);
  CHECK(published == &a);
  CHECK(queued == &b);
  pthread_mutex_unlock(&cache.mu);

  pthread_join(a_thread, NULL);
  pthread_join(b_thread, NULL);
  alarm(0);
  signal(SIGALRM, old_alarm_handler);

  CHECK(atomic_load_explicit(&a_runner.returned, memory_order_acquire));
  CHECK(atomic_load_explicit(&b_runner.returned, memory_order_acquire));
  CHECK(atomic_load_explicit(&a_progress, memory_order_relaxed) == 1);
  CHECK(atomic_load_explicit(&b_progress, memory_order_relaxed) == 1);
  CHECK(a_batch.len == 1 && a_batch.xs[0].score == 1);
  CHECK(b_batch.len == 1 && b_batch.xs[0].score == 1);
  CHECK(pool->epoch == 2);

  batch_cache_release_query(&cache, target);
  batch_cache_free(&cache);
  async_worker_pool_destroy(pool);
}

static void test_async_worker_pool_round_robins_overlapping_sessions(void) {
  /* With one worker, A's first batch is held in the cache insertion while B
     joins the FIFO.  Once released, A must rejoin behind B after its one-batch
     quantum.  B therefore returns before A drains its remaining two batches.
     The old whole-job scheduler always returned A first. */
  void (*old_alarm_handler)(int) = signal(SIGALRM, test_pool_overlap_timeout);
  alarm(5);

  struct AsyncWorkerPool *pool = async_worker_pool_create(1);
  CHECK(pool != NULL);
  if (!pool) {
    alarm(0);
    signal(SIGALRM, old_alarm_handler);
    return;
  }

  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *target = batch_cache_acquire_query(
      &cache, "", CaseSmart, true);
  CHECK(target != NULL);
  if (!target) {
    async_worker_pool_destroy(pool);
    alarm(0);
    signal(SIGALRM, old_alarm_handler);
    return;
  }

  struct AsyncScoringBatch a_batches[3] = {
    {.len = 1, .batch_id = 0, .cacheable = true,
     .xs = {{.str = "alpha", .idx = 0}}},
    {.len = 1, .batch_id = 1, .cacheable = true,
     .xs = {{.str = "beta", .idx = 1}}},
    {.len = 1, .batch_id = 2, .cacheable = true,
     .xs = {{.str = "gamma", .idx = 2}}},
  };
  struct AsyncScoringBatch b_batch = {
    .len = 1,
    .xs = {{.str = "small", .idx = 0}},
  };
  _Atomic bool a_stop = false, b_stop = false;
  _Atomic size_t a_progress = 0, b_progress = 0;
  struct AsyncScoringShared a = {
    .pattern = NULL,
    .batches = a_batches,
    .remaining = 3,
    .stop = &a_stop,
    .progress_completed = &a_progress,
    .batch_cache = &cache,
    .target_query = target,
  };
  struct AsyncScoringShared b = {
    .pattern = NULL,
    .batches = &b_batch,
    .remaining = 1,
    .stop = &b_stop,
    .progress_completed = &b_progress,
  };
  _Atomic unsigned completion_sequence = 0;
  struct TestPoolRunner a_runner = {
    .pool = pool, .job = &a, .completion_sequence = &completion_sequence};
  struct TestPoolRunner b_runner = {
    .pool = pool, .job = &b, .completion_sequence = &completion_sequence};
  pthread_t a_thread, b_thread;

  pthread_mutex_lock(&cache.mu);
  CHECK(pthread_create(&a_thread, NULL, test_pool_runner, &a_runner) == 0);
  double deadline = monotonic_ms() + 1000.0;
  while (atomic_load_explicit(&a.remaining, memory_order_acquire) != 2 &&
         monotonic_ms() < deadline)
    test_sleep_ms(1);
  CHECK(atomic_load_explicit(&a.remaining, memory_order_acquire) == 2);

  CHECK(pthread_create(&b_thread, NULL, test_pool_runner, &b_runner) == 0);
  deadline = monotonic_ms() + 1000.0;
  size_t waiting = 0;
  do {
    pthread_mutex_lock(&pool->mu);
    waiting = pool->waiting;
    pthread_mutex_unlock(&pool->mu);
    if (waiting == 1) break;
    test_sleep_ms(1);
  } while (monotonic_ms() < deadline);
  CHECK(waiting == 1);
  pthread_mutex_unlock(&cache.mu);

  pthread_join(a_thread, NULL);
  pthread_join(b_thread, NULL);
  alarm(0);
  signal(SIGALRM, old_alarm_handler);

  CHECK(atomic_load_explicit(&b_runner.return_order,
                             memory_order_acquire) == 1);
  CHECK(atomic_load_explicit(&a_runner.return_order,
                             memory_order_acquire) == 2);
  CHECK(atomic_load_explicit(&a_progress, memory_order_relaxed) == 3);
  CHECK(atomic_load_explicit(&b_progress, memory_order_relaxed) == 1);
  /* A yields once to B, then drains its final two batches without another
     barrier after contention disappears. */
  CHECK(pool->epoch == 3);

  batch_cache_release_query(&cache, target);
  batch_cache_free(&cache);
  async_worker_pool_destroy(pool);
}

static void test_async_worker_pool_unlinks_cancelled_waiter(void) {
  void (*old_alarm_handler)(int) = signal(SIGALRM, test_pool_overlap_timeout);
  alarm(5);

  struct AsyncWorkerPool *pool = async_worker_pool_create(1);
  CHECK(pool != NULL);
  if (!pool) {
    alarm(0);
    signal(SIGALRM, old_alarm_handler);
    return;
  }
  BatchCache cache;
  batch_cache_init(&cache, 1024 * 1024);
  BatchQuery *target = batch_cache_acquire_query(
      &cache, "", CaseSmart, true);
  CHECK(target != NULL);
  if (!target) {
    async_worker_pool_destroy(pool);
    alarm(0);
    signal(SIGALRM, old_alarm_handler);
    return;
  }

  struct AsyncScoringBatch a_batch = {
    .len = 1,
    .cacheable = true,
    .xs = {{.str = "alpha", .idx = 0}},
  };
  struct AsyncScoringBatch b_batch = {
    .len = 1,
    .xs = {{.str = "beta", .idx = 0}},
  };
  _Atomic bool a_stop = false, b_stop = false;
  _Atomic size_t a_progress = 0, b_progress = 0;
  struct AsyncScoringShared a = {
    .batches = &a_batch,
    .remaining = 1,
    .stop = &a_stop,
    .progress_completed = &a_progress,
    .batch_cache = &cache,
    .target_query = target,
  };
  struct AsyncScoringShared b = {
    .batches = &b_batch,
    .remaining = 1,
    .stop = &b_stop,
    .progress_completed = &b_progress,
  };
  struct TestPoolRunner a_runner = {.pool = pool, .job = &a};
  struct TestPoolRunner b_runner = {.pool = pool, .job = &b};
  pthread_t a_thread, b_thread;

  pthread_mutex_lock(&cache.mu);
  CHECK(pthread_create(&a_thread, NULL, test_pool_runner, &a_runner) == 0);
  double deadline = monotonic_ms() + 1000.0;
  while (atomic_load_explicit(&a.remaining, memory_order_acquire) != 0 &&
         monotonic_ms() < deadline)
    test_sleep_ms(1);
  CHECK(atomic_load_explicit(&a.remaining, memory_order_acquire) == 0);
  CHECK(pthread_create(&b_thread, NULL, test_pool_runner, &b_runner) == 0);

  deadline = monotonic_ms() + 1000.0;
  size_t waiting = 0;
  do {
    pthread_mutex_lock(&pool->mu);
    waiting = pool->waiting;
    pthread_mutex_unlock(&pool->mu);
    if (waiting == 1) break;
    test_sleep_ms(1);
  } while (monotonic_ms() < deadline);
  CHECK(waiting == 1);

  atomic_store_explicit(&b_stop, true, memory_order_release);
  async_worker_pool_wake(pool);
  deadline = monotonic_ms() + 500.0;
  while (!atomic_load_explicit(&b_runner.returned, memory_order_acquire) &&
         monotonic_ms() < deadline)
    test_sleep_ms(1);
  CHECK(atomic_load_explicit(&b_runner.returned, memory_order_acquire));
  CHECK(atomic_load_explicit(&b_progress, memory_order_relaxed) == 0);
  CHECK(pool->epoch == 1);

  pthread_mutex_unlock(&cache.mu);
  pthread_join(a_thread, NULL);
  pthread_join(b_thread, NULL);
  alarm(0);
  signal(SIGALRM, old_alarm_handler);
  CHECK(atomic_load_explicit(&a_progress, memory_order_relaxed) == 1);
  CHECK(pool->epoch == 1);
  CHECK(pool->waiting == 0);

  batch_cache_release_query(&cache, target);
  batch_cache_free(&cache);
  async_worker_pool_destroy(pool);
}

/* =====================================================================
 * async_session_destroy_async (off-main detached teardown)
 * ===================================================================== */

/* Reader stub: park until s->stop is signaled, then return.  Acts as a
   stand-in for the real `async_reader' so the destroy path has a
   join-able thread without us having to fork a subprocess. */
static void *test_destroy_reader_stub(void *arg) {
  AsyncSession *s = arg;
  while (!atomic_load_explicit(&s->stop, memory_order_relaxed))
    test_sleep_ms(1);
  return NULL;
}

/* Score stub: park on the request cond until score_req_stop is set,
   mirroring `scoring_thread_fn's idle wait state at session start. */
static void *test_destroy_score_stub(void *arg) {
  AsyncSession *s = arg;
  pthread_mutex_lock(&s->score_req_mu);
  while (!s->score_req_stop)
    pthread_cond_wait(&s->score_req_cond, &s->score_req_mu);
  pthread_mutex_unlock(&s->score_req_mu);
  return NULL;
}

/* Build a minimally-initialized session with real worker threads so
   `async_session_destroy_async' has something join-able to tear down. */
static AsyncSession *make_destroy_test_session(void) {
  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  atomic_init(&s->lifetime_refs, 1);
  atomic_init(&s->handle_owner_released, false);
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->child_mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  cache_init(&s->cache, 4);
  s->pid = -1;        /* no real subprocess */
  s->fp  = NULL;
  if (pthread_create(&s->reader, NULL, test_destroy_reader_stub, s) != 0)
    return NULL;
  s->reader_started = true;
  if (pthread_create(&s->score_thread, NULL, test_destroy_score_stub, s) != 0)
    return NULL;
  s->score_thread_started = true;
  return s;
}

/* Regression for a producer that closes stdout but remains alive.  The
   reader owns waitpid after EOF; stop must nevertheless signal the child
   instead of blocking destruction until the child exits by itself. */
static void test_startup_cleanup_kills_term_resistant_child(void) {
  int ready[2];
  CHECK(pipe(ready) == 0);
  pid_t pid = fork();
  CHECK(pid >= 0);
  if (pid < 0) {
    close(ready[0]);
    close(ready[1]);
    return;
  }
  if (pid == 0) {
    (void)setpgid(0, 0);
    (void)signal(SIGTERM, SIG_IGN);
    close(ready[0]);
    char byte = 'R';
    (void)write(ready[1], &byte, 1);
    close(ready[1]);
    for (;;) pause();
  }

  (void)setpgid(pid, pid);
  close(ready[1]);
  char byte = 0;
  ssize_t amount;
  do {
    amount = read(ready[0], &byte, 1);
  } while (amount < 0 && errno == EINTR);
  close(ready[0]);
  CHECK(amount == 1 && byte == 'R');

  double t0 = monotonic_ms();
  async_kill_and_reap_spawned_child(pid);
  CHECK(monotonic_ms() - t0 < 1000.0);
  errno = 0;
  CHECK(waitpid(pid, NULL, WNOHANG) == -1);
  CHECK(errno == ECHILD);
}

static void test_destroy_signals_reader_owned_child(void) {
  int pfd[2];
  CHECK(pipe(pfd) == 0);
  pid_t pid = fork();
  CHECK(pid >= 0);
  if (pid == 0) {
    fzf_prepare_forked_child_signals();
    (void)setpgid(0, 0);
    close(pfd[0]);
    close(pfd[1]);
    test_sleep_ms(3000);
    _exit(0);
  }
  (void)setpgid(pid, pid);
  close(pfd[1]);

  AsyncSession *s = calloc(1, sizeof *s);
  CHECK(s != NULL);
  s->pid = pid;
  s->fp = fdopen(pfd[0], "r");
  CHECK(s->fp != NULL);
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->child_mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  cache_init(&s->cache, 4);
  batch_cache_init(&s->batch_cache, 0);
  s->child_alive = true;
  atomic_store_explicit(&s->child_owner, AsyncChildUnclaimed,
                        memory_order_relaxed);
  CHECK(pthread_create(&s->reader, NULL, async_reader, s) == 0);
  s->reader_started = true;

  double deadline = monotonic_ms() + 1000.0;
  while (atomic_load_explicit(&s->child_owner, memory_order_acquire) !=
             AsyncChildReader && monotonic_ms() < deadline)
    test_sleep_ms(1);
  CHECK(atomic_load_explicit(&s->child_owner, memory_order_acquire) ==
        AsyncChildReader);
  CHECK(kill(pid, 0) == 0);

  double t0 = monotonic_ms();
  async_session_destroy(s);
  CHECK(monotonic_ms() - t0 < 1000.0);
  errno = 0;
  CHECK(waitpid(pid, NULL, WNOHANG) == -1);
  CHECK(errno == ECHILD);
}

/* A descendant can escape the producer process group with setsid() and retain
   stdout forever.  Teardown must wake the reader through its own cancellation
   pipe rather than waiting for process-tree cleanup to close that descriptor. */
static void test_destroy_wakes_reader_with_escaped_descendant(void) {
  int output[2], escaped_pid_pipe[2], ready_pipe[2];
  CHECK(pipe(output) == 0);
  CHECK(pipe(escaped_pid_pipe) == 0);
  CHECK(pipe(ready_pipe) == 0);

  pid_t producer = fork();
  CHECK(producer >= 0);
  if (producer == 0) {
    (void)setpgid(0, 0);
    close(output[0]);
    close(escaped_pid_pipe[0]);
    close(ready_pipe[0]);
    pid_t escaped = fork();
    if (escaped < 0) _exit(100);
    if (escaped == 0) {
      if (setsid() < 0) _exit(101);
      pid_t self = getpid();
      if (write(escaped_pid_pipe[1], &self, sizeof self) != sizeof self)
        _exit(102);
      if (write(output[1], "alpha\nomega", 11) != 11) _exit(103);
      if (write(ready_pipe[1], "R", 1) != 1) _exit(104);
      close(escaped_pid_pipe[1]);
      close(ready_pipe[1]);
      for (;;) pause();
    }
    close(output[1]);
    close(escaped_pid_pipe[1]);
    close(ready_pipe[1]);
    _exit(0);
  }

  (void)setpgid(producer, producer);
  close(output[1]);
  close(escaped_pid_pipe[1]);
  close(ready_pipe[1]);
  pid_t escaped = -1;
  char ready = 0;
  CHECK(read(escaped_pid_pipe[0], &escaped, sizeof escaped) == sizeof escaped);
  CHECK(read(ready_pipe[0], &ready, 1) == 1 && ready == 'R');
  close(escaped_pid_pipe[0]);
  close(ready_pipe[0]);

  AsyncSession *s = calloc(1, sizeof *s);
  CHECK(s != NULL);
  s->pid = producer;
  s->fp = fdopen(output[0], "r");
  CHECK(s->fp != NULL);
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->child_mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  cache_init(&s->cache, 4);
  batch_cache_init(&s->batch_cache, 0);
  s->child_alive = true;
  atomic_store_explicit(&s->child_owner, AsyncChildUnclaimed,
                        memory_order_relaxed);
  CHECK(async_make_cancel_pipe(s));
  CHECK(pthread_create(&s->reader, NULL, async_reader, s) == 0);
  s->reader_started = true;

  double deadline = monotonic_ms() + 1000.0;
  while (async_observe_pool(s).count < 1 && monotonic_ms() < deadline)
    test_sleep_ms(1);
  CHECK(async_observe_pool(s).count == 1);
  CHECK(strcmp(cands_at(s, 0), "alpha") == 0);
  CHECK(kill(escaped, 0) == 0);

  double t0 = monotonic_ms();
  async_session_destroy(s);
  CHECK(monotonic_ms() - t0 < 1000.0);
  CHECK(kill(escaped, 0) == 0);

  CHECK(kill(escaped, SIGKILL) == 0);
  deadline = monotonic_ms() + 5000.0;
  while (kill(escaped, 0) == 0 && monotonic_ms() < deadline)
    test_sleep_ms(5);
  errno = 0;
  CHECK(kill(escaped, 0) == -1);
  CHECK(errno == ESRCH);
}

static double monotonic_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

static void test_sleep_ms(long milliseconds) {
  struct timespec remaining = {
      .tv_sec = milliseconds / 1000,
      .tv_nsec = (milliseconds % 1000) * 1000000,
  };
  while (nanosleep(&remaining, &remaining) != 0 && errno == EINTR) {}
}

static void test_destroy_async_returns_fast(void) {
  AsyncSession *s = make_destroy_test_session();
  CHECK(s != NULL);
  uint64_t base = atomic_load_explicit(&async_destroy_completions,
                                       memory_order_relaxed);

  double t0 = monotonic_ms();
  async_session_destroy_async(s);
  double elapsed = monotonic_ms() - t0;

  /* Returning fast is the entire point — the caller (Emacs main on
     minibuffer exit) must not block on pthread_join.  20 ms gives wide
     headroom over expected ~µs while still catching a regression where
     someone wires the synchronous destroy back into the stop path. */
  CHECK(elapsed < 20.0);

  /* Detached worker eventually completes; wait up to 5 s. */
  double deadline = monotonic_ms() + 5000.0;
  while (atomic_load_explicit(&async_destroy_completions,
                              memory_order_relaxed) == base
         && monotonic_ms() < deadline) {
    test_sleep_ms(5);
  }
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) == base + 1);
}

static void test_destroy_async_handles_null(void) {
  /* Defensive: NULL handle must be a no-op (matches the user_ptr-after-
     stop GC path, where the finalizer sees nullptr). */
  uint64_t base = atomic_load_explicit(&async_destroy_completions,
                                       memory_order_relaxed);
  async_session_destroy_async(NULL);
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) == base);
}

static void test_handle_release_waits_for_inflight_api_pin(void) {
  AsyncSession *s = make_destroy_test_session();
  CHECK(s != NULL);
  uint64_t base = atomic_load_explicit(&async_destroy_completions,
                                       memory_order_relaxed);

  /* Model an outer module call that has obtained the user pointer and can
     now invoke reentrant Lisp.  Stop detaches the handle and wakes all native
     work immediately, but it must not free S until the outer call unpins. */
  async_session_retain(s);
  async_session_release_handle_owner(s);
  CHECK(atomic_load_explicit(&s->stop, memory_order_acquire));
  CHECK(atomic_load_explicit(&s->handle_owner_released,
                             memory_order_acquire));
  test_sleep_ms(25);
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) == base);

  /* Finalizer-after-explicit-stop is harmless even if an embedding invokes
     it with the old payload rather than the handle's new NULL payload. */
  async_session_release_handle_owner(s);
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) == base);

  async_session_release(s);
  double deadline = monotonic_ms() + 5000.0;
  while (atomic_load_explicit(&async_destroy_completions,
                              memory_order_relaxed) == base &&
         monotonic_ms() < deadline)
    test_sleep_ms(5);
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) == base + 1);
}

static void test_destroy_async_many_in_flight(void) {
  /* Multi-source scenario: N sessions destroyed back-to-back from main
     must collectively return fast — proves the cost is per-call
     pthread_create, not summed pthread_join time. */
  enum { N = 8 };
  AsyncSession *sessions[N];
  for (int i = 0; i < N; i++) {
    sessions[i] = make_destroy_test_session();
    CHECK(sessions[i] != NULL);
  }
  uint64_t base = atomic_load_explicit(&async_destroy_completions,
                                       memory_order_relaxed);
  double t0 = monotonic_ms();
  for (int i = 0; i < N; i++) async_session_destroy_async(sessions[i]);
  double elapsed = monotonic_ms() - t0;
  CHECK(elapsed < 50.0);

  double deadline = monotonic_ms() + 5000.0;
  while (atomic_load_explicit(&async_destroy_completions,
                              memory_order_relaxed) - base < (uint64_t)N
         && monotonic_ms() < deadline) {
    test_sleep_ms(5);
  }
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) - base == (uint64_t)N);
}

/* fzf_parse_pattern used strtok before the interactive session work added
   concurrent parsing on the submission and scoring threads.  strtok keeps a
   process-global cursor.  One parser could therefore continue in a buffer
   that another parser had already freed.  This stress case gives ASan and
   TSan a focused reproducer without depending on a libFuzzer schedule. */
struct ConcurrentParseState {
  size_t offset;
  int failed;
};

static void *concurrent_parse_worker(void *opaque) {
  static const char *queries[] = {
      "alpha beta", "^foo | bar$", "!zzz qux", "'fuzzy exact",
      "你 好", "K k",
  };
  static const size_t expected_sets[] = {2, 1, 2, 2, 2, 2};
  static const size_t expected_terms[] = {2, 2, 2, 2, 2, 2};
  struct ConcurrentParseState *state = opaque;

  for (size_t iteration = 0; iteration < 4000; iteration++) {
    size_t query_index = (iteration + state->offset) %
                         (sizeof queries / sizeof queries[0]);
    char *query = strdup(queries[query_index]);
    if (!query) {
      state->failed = 1;
      return NULL;
    }

    fzf_pattern_t *pattern = fzf_parse_pattern(
        CaseSmart, false, query, (iteration & 1) != 0);
    free(query);
    if (!pattern || pattern->size != expected_sets[query_index]) {
      state->failed = 1;
      if (pattern) fzf_free_pattern(pattern);
      return NULL;
    }

    size_t terms = 0;
    for (size_t set_index = 0; set_index < pattern->size; set_index++) {
      fzf_term_set_t *set = pattern->ptr[set_index];
      if (!set || set->size == 0) {
        state->failed = 1;
        break;
      }
      terms += set->size;
    }
    if (terms != expected_terms[query_index]) state->failed = 1;
    fzf_free_pattern(pattern);
    if (state->failed) return NULL;
  }
  return NULL;
}

static void test_parse_pattern_concurrently(void) {
  enum { thread_count = 8 };
  pthread_t threads[thread_count];
  struct ConcurrentParseState states[thread_count] = {0};
  size_t created = 0;

  for (size_t i = 0; i < thread_count; i++) {
    states[i].offset = i;
    if (pthread_create(&threads[i], NULL, concurrent_parse_worker,
                       &states[i]) != 0) {
      CHECK(false);
      break;
    }
    created++;
  }
  for (size_t i = 0; i < created; i++) pthread_join(threads[i], NULL);
  for (size_t i = 0; i < created; i++) CHECK(states[i].failed == 0);
}

/* ================================================================= */

/* Review LT-1 / KK2-3 regression matrix: an undecodable byte must not
   make the rest of the candidate unmatchable, and highlight positions
   on such candidates must stay aligned with the one-char-per-raw-byte
   counting Emacs uses. */
static int32_t lossy_score_of(const char *text, const char *pattern_str) {
  char *dup = strdup(pattern_str);
  fzf_pattern_t *p = fzf_parse_pattern(CaseSmart, false, dup, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  int32_t score = fzf_get_score(text, p, slab);
  fzf_free_pattern(p);
  fzf_free_slab(slab);
  free(dup);
  return score;
}

static void test_invalid_utf8_candidates_stay_matchable(void) {
  CHECK(lossy_score_of("src/caf\xe9/main.c", "main") > 0);
  CHECK(lossy_score_of("src/caf\xe9/main.c", "srcmain") > 0);
  CHECK(lossy_score_of("src/caf\xe9/main.c", "src") > 0);
  CHECK(lossy_score_of("ab\x80" "cdefgz", "fgz") > 0);
  CHECK(lossy_score_of("abc\xe4\xb8" "def", "def") > 0);
  CHECK(lossy_score_of("abcd\xff" "efg", "efg") > 0);
  CHECK(lossy_score_of("src/caf\xe9/main.c", "main.c$") > 0);
  CHECK(lossy_score_of("src/caf\xe9/main.c", "^src") > 0);
  /* The same raw byte matches itself, and never matches the codepoint
     it would spell in Latin-1. */
  CHECK(lossy_score_of("caf\xe9 latte", "f\xe9 l") > 0);
  CHECK(lossy_score_of("caf\xe9", "caf\xc3\xa9") == 0);
  CHECK(lossy_score_of("caf\xc3\xa9 zz", "caf\xc3\xa9") > 0);
}

static void test_invalid_utf8_highlight_positions(void) {
  /* "ca<E9>zzQR": chars c=0 a=1 <raw>=2 z=3 z=4 Q=5 R=6. */
  const char *cand = "ca\xe9zzQR";
  static const struct { const char *q; uint32_t lo, hi; } cases[] = {
    { "'QR", 5, 6 }, { "QR$", 5, 6 }, { "^ca", 0, 1 }, { "QR", 5, 6 },
  };
  for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
    char *dup = strdup(cases[i].q);
    fzf_pattern_t *p = fzf_parse_pattern(CaseSmart, false, dup, true);
    fzf_slab_t *slab = fzf_make_default_slab();
    fzf_position_t *pos = fzf_get_positions(cand, p, slab);
    CHECK(pos && pos->size == 2);
    if (pos && pos->size == 2) {
      uint32_t lo = pos->data[0] < pos->data[1] ? pos->data[0] : pos->data[1];
      uint32_t hi = pos->data[0] < pos->data[1] ? pos->data[1] : pos->data[0];
      CHECK(lo == cases[i].lo);
      CHECK(hi == cases[i].hi);
    }
    if (pos) fzf_free_positions(pos);
    fzf_free_pattern(p);
    fzf_free_slab(slab);
    free(dup);
  }
}

static void test_suffix_pattern_longer_than_candidate(void) {
  /* Review DL2-3: the `trimmed_len < M' guard in fzf_suffix_match was
     gated by nothing -- deleting it underflows `trimmed_len - M' and
     reads far out of bounds.  The corpus seed
     fuzz/corpus/suffix-pattern-longer-than-candidate (`bd$') gates the
     overflow under ASan; this is the deterministic no-match half. */
  CHECK(lossy_score_of("b", "bd$") == 0);
  CHECK(lossy_score_of("", "bd$") == 0);
  CHECK(lossy_score_of("d", "abcd$") == 0);
  CHECK(lossy_score_of("caf\xe9", "caf\xe9zz$") == 0);
}

static void test_worker_count_bounds_fallible_cpu_probe(void) {
  CHECK(fzf_worker_count(0, 4) == 1);
  CHECK(fzf_worker_count(-1, 4) == 1);
  CHECK(fzf_worker_count(1, 4) == 1);
  CHECK(fzf_worker_count(8, 4) == 4);
  CHECK(fzf_worker_count(LONG_MAX, 0) == ASYNC_WORKER_LIMIT);
  CHECK(fzf_worker_count(LONG_MAX, 3) == 3);
}

int main(void) {
  printf("--- counting_sort_candidates ---\n");
  RUN(test_n_zero);
  RUN(test_n_one);
  RUN(test_small_n_insertion_sort);
  RUN(test_small_n_stability);
  RUN(test_large_n_correctness);
  RUN(test_stability_with_ties);
  RUN(test_all_same_score);
  RUN(test_all_zero_score);
  RUN(test_matches_qsort);

  printf("--- counting_sort_scored ---\n");
  RUN(test_scored_n_zero);
  RUN(test_scored_n_one);
  RUN(test_scored_large_n_correctness);
  RUN(test_scored_stability);
  RUN(test_scored_matches_qsort);
  RUN(test_bounded_top_k_matches_full_stable_sort);
  RUN(test_membership_cap_discards_incomplete_prefix);
  RUN(test_top_k_finalization_observes_cancellation);
  RUN(test_allocationless_sort_matches_total_order);

  printf("--- async_strip_ansi ---\n");
  RUN(test_strip_ansi_no_escape);
  RUN(test_strip_ansi_simple_color);
  RUN(test_strip_ansi_multiple_sequences);
  RUN(test_strip_ansi_bare_esc);

  printf("--- async_reader ---\n");
  RUN(test_async_reader_basic);
  RUN(test_async_reader_coalesces_growth_after_request);
  RUN(test_async_reader_publishes_terminal_generation);
  RUN(test_async_reader_ansi_stripping);
  RUN(test_async_reader_preserves_empty_records);
  RUN(test_async_reader_many_lines);
  RUN(test_async_reader_long_line);
  RUN(test_async_reader_final_unterminated_line);
  RUN(test_async_reader_rejects_embedded_nul);

  printf("--- chunked cands_top ---\n");
  RUN(test_cands_top_index_split);
  RUN(test_cands_top_accessor_reads_block_pointer);

  printf("--- cache (phase 1: exact-match) ---\n");
  RUN(test_cache_lookup_miss_on_empty);
  RUN(test_cache_insert_then_lookup_hit);
  RUN(test_cache_lookup_miss_distinct_query);
  RUN(test_cache_insert_updates_in_place);
  RUN(test_cache_lru_eviction_at_capacity);
  RUN(test_cache_touch_on_hit);
  RUN(test_cache_insert_zero_count);
  RUN(test_cache_evicts_to_byte_budget);
  RUN(test_cache_skips_single_oversize_entry);
  RUN(test_cache_disabled_skips_insert_work);
  RUN(test_cache_pool_gen_distinguishes_stale);
  RUN(test_cache_exact_separates_case_and_fuzzy_modes);
  RUN(test_cache_exact_requires_sufficient_result_capacity);
  RUN(test_cache_exact_caps_result_to_requested_limit);
  RUN(test_cache_exact_filter_only_requires_same_emit_window);
  RUN(test_cache_exact_reuses_membership_across_filter_only_mode);

  printf("--- cache (phase 2: term-set subsumption) ---\n");
  RUN(test_subsumes_pattern_extending_term_via_byte_prefix);
  RUN(test_byte_prefix_rejects_operator_source_terms);
  RUN(test_subsumes_pattern_adding_term_at_end);
  RUN(test_subsumes_pattern_adding_term_at_start);
  RUN(test_subsumes_pattern_term_reorder);
  RUN(test_subsumes_pattern_negation_at_start);
  RUN(test_subsumes_pattern_or_query_rejected);
  RUN(test_subsumes_pattern_distinct_terms);
  RUN(test_cache_lookup_prefix_v2_finds_term_subset);
  RUN(test_cache_lookup_prefix_rejects_inverse_extension);
  RUN(test_cache_lookup_prefix_v2_finds_reordered);
  RUN(test_cache_lookup_prefix_picks_most_terms);
  RUN(test_cache_lookup_prefix_skips_or_in_query);
  RUN(test_cache_lookup_prefix_skips_exact_match);

  printf("--- stable batch membership cache ---\n");
  RUN(test_batch_cache_sparse_bitmap_and_selectivity_cutoff);
  RUN(test_batch_cache_selects_only_safe_query_ancestors);
  RUN(test_batch_cache_evicts_to_byte_budget);
  RUN(test_completed_batch_evidence_survives_request_cancellation);
  RUN(test_mutable_partial_batch_does_not_enter_batch_cache);

  printf("--- interactive request identity ---\n");
  RUN(test_session_abi_is_versioned);
  RUN(test_async_snapshot_staleness_covers_pool_growth);
  RUN(test_batch_worker_stops_on_shared_allocation_failure);
  RUN(test_copy_emacs_string_fallback_is_bounded);
  RUN(test_lossless_string_conversion_preserves_runtime_errors);
  RUN(test_startup_posix_errors_are_descriptive);
  RUN(test_async_start_publishes_inert_handle_before_resources);
  RUN(test_status_metadata_does_not_copy_results);
  RUN(test_snapshot_capture_uses_live_pool_boundary);
  RUN(test_snapshot_copy_oom_is_not_authoritative_empty);
  RUN(test_snapshot_copy_owns_candidate_strings);
  RUN(test_snapshot_string_copy_oom_is_not_authoritative_empty);
  RUN(test_growth_retry_query_oom_is_terminal);
  RUN(test_async_request_state_precedence);
  RUN(test_async_batch_window_bounds_preparation_memory);
  RUN(test_async_pool_observation_is_terminally_consistent);
  RUN(test_matcher_failure_retains_last_completed_result);
  RUN(test_async_request_identity_includes_matching_options);
  RUN(test_async_submit_oom_does_not_publish_request);
  RUN(test_async_running_request_reuse_requires_latest_slot);
  RUN(test_async_worker_pool_reuses_threads_across_rounds);
  RUN(test_async_worker_pool_serializes_overlapping_sessions);
  RUN(test_async_worker_pool_round_robins_overlapping_sessions);
  RUN(test_async_worker_pool_unlinks_cancelled_waiter);

  printf("--- concurrent pattern parsing ---\n");
  RUN(test_parse_pattern_concurrently);

  printf("--- async_session_destroy_async ---\n");
  RUN(test_destroy_async_handles_null);
  RUN(test_handle_release_waits_for_inflight_api_pin);
  RUN(test_destroy_async_returns_fast);
  RUN(test_destroy_async_many_in_flight);
  RUN(test_startup_cleanup_kills_term_resistant_child);
  RUN(test_destroy_signals_reader_owned_child);
  RUN(test_destroy_wakes_reader_with_escaped_descendant);

  RUN(test_invalid_utf8_candidates_stay_matchable);
  RUN(test_invalid_utf8_highlight_positions);
  RUN(test_suffix_pattern_longer_than_candidate);
  RUN(test_worker_count_bounds_fallible_cpu_probe);

  if (failed == 0) {
    printf("\nAll tests passed.\n");
    return 0;
  } else {
    printf("\n%d check(s) failed.\n", failed);
    return 1;
  }
}
