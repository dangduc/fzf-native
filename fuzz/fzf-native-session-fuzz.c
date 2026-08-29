/* SPDX-License-Identifier: GPL-3.0-or-later
 * Coverage-guided state-machine fuzzer for fzf-native interactive sessions.
 *
 * This target includes the real native module implementation through the same
 * plain-C path as fzf-native-ctest.c.  A compact bytecode drives candidate
 * growth, request submission, cancellation, polling, cache reuse, worker
 * scoring, publication, and teardown without requiring an Emacs process.
 */

#define FZF_NATIVE_CTEST 1
#include "../fzf-native-module.c"

#include <sched.h>
#include <time.h>

enum {
  SESSION_FUZZ_MAX_INPUT = 8192,
  SESSION_FUZZ_MAX_OPERATIONS = 128,
  SESSION_FUZZ_MAX_CANDIDATES = 4097,
};

typedef struct {
  const uint8_t *data;
  size_t size;
  size_t offset;
} FuzzInput;

typedef struct {
  AsyncSession *session;
  uint64_t observed_result_id;
  uint64_t observed_snapshot_generation;
  size_t operation;
  bool submitted;
} FuzzSession;

static ptrdiff_t session_fuzz_line_limit(uint8_t options) {
  static const ptrdiff_t limits[] = {0, 1, -1, 2, -2, 8, -8, 256};
  return limits[(options >> 1) & 7];
}

static void session_fuzz_fail(const FuzzSession *fuzz, const char *property) {
  fprintf(stderr,
          "fzf-native interactive fuzz invariant failed at operation %zu: %s\n",
          fuzz ? fuzz->operation : 0, property);
  abort();
}

static void session_fuzz_check_candidate_analyzer(
    const uint8_t *data, size_t size) {
  bool actual_ascii = false;
  bool actual_valid = str_analyze_candidate(
      (struct Str){.b = (char *)data, .len = size}, &actual_ascii);
  bool expected_valid = data != NULL || size == 0;
  bool expected_ascii = true;
  if (data) {
    for (size_t i = 0; i < size; i++) {
      if (data[i] == 0) expected_valid = false;
      if (data[i] & 0x80) expected_ascii = false;
    }
  }
  if (actual_valid != expected_valid)
    session_fuzz_fail(NULL, "candidate validation disagrees with scalar oracle");
  if (actual_valid && actual_ascii != expected_ascii)
    session_fuzz_fail(NULL, "candidate classification disagrees with scalar oracle");
}

static uint8_t fuzz_take(FuzzInput *input) {
  return input->offset < input->size ? input->data[input->offset++] : 0;
}

static size_t fuzz_remaining(const FuzzInput *input) {
  return input->offset < input->size ? input->size - input->offset : 0;
}

static AsyncSession *session_fuzz_create(uint8_t options) {
  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) return NULL;

  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->child_mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  atomic_store(&s->child_owner, AsyncChildUnclaimed);
  atomic_store(&s->producer_state, AsyncProducerRunning);
  atomic_store(&s->producer_error, 0);
  atomic_store(&s->producer_exit_status, -1);

  static const size_t batch_cache_sizes[] = {
      0, 4096, 64 * 1024, 1024 * 1024,
  };
  static const size_t filter_only_thresholds[] = {
      0, 1, 128, BATCH_SIZE,
  };
  cache_init(&s->cache, 1 + ((options >> 1) & 7));
  batch_cache_init(
      &s->batch_cache,
      batch_cache_sizes[(options >> 4) & 3]);
  if (s->batch_cache.max_bytes) {
    /* Production retains thousands of query records.  Small fuzz-only
       limits make query eviction and bounded ancestor scans reachable within
       the 128-operation state-machine budget. */
    s->batch_cache.max_queries = 1 + ((options >> 1) & 7);
    s->batch_cache.source_scan_limit = 1 + ((options >> 4) & 7);
  }
  s->filter_only_min_pool = filter_only_thresholds[(options >> 6) & 3];
  s->max_line_length = session_fuzz_line_limit(options);
  s->worker_pool = async_worker_pool_create(1 + (options & 1));
  if (!s->worker_pool) {
    async_session_destroy(s);
    return NULL;
  }
  s->worker_pool_owned = true;
  if (pthread_create(&s->score_thread, NULL, scoring_thread_fn, s) != 0) {
    async_session_destroy(s);
    return NULL;
  }
  s->score_thread_started = true;
  return s;
}

static bool session_fuzz_write_all(int fd, const uint8_t *data, size_t size) {
  size_t offset = 0;
  while (offset < size) {
    ssize_t written = write(fd, data + offset, size - offset);
    if (written > 0) {
      offset += (size_t)written;
      continue;
    }
    if (written < 0 && errno == EINTR) continue;
    return false;
  }
  return true;
}

typedef struct {
  char **candidates;
  size_t count;
  size_t capacity;
  bool invalid_data;
} FuzzReaderReference;

static void session_fuzz_reader_reference_free(FuzzReaderReference *ref) {
  for (size_t i = 0; i < ref->count; i++) free(ref->candidates[i]);
  free(ref->candidates);
  memset(ref, 0, sizeof *ref);
}

static bool session_fuzz_reader_reference_append(FuzzReaderReference *ref,
                                                 char *candidate) {
  if (ref->count == ref->capacity) {
    size_t next = ref->capacity ? ref->capacity * 2 : 8;
    char **grown = realloc(ref->candidates, next * sizeof *grown);
    if (!grown) return false;
    ref->candidates = grown;
    ref->capacity = next;
  }
  ref->candidates[ref->count++] = candidate;
  return true;
}

/* Independent one-shot oracle for the incremental reader normalizer.  Fuzz
   inputs are small enough to retain whole records here; production must not.
   This checks the streaming CR/ANSI/UTF-8 boundary logic against the prior
   semantics for every generated record and cap mode. */
static bool session_fuzz_reader_reference(const uint8_t *data, size_t size,
                                          ptrdiff_t max_line_length,
                                          FuzzReaderReference *ref) {
  size_t start = 0;
  while (start < size) {
    size_t end = start;
    while (end < size && data[end] != '\n') end++;
    bool terminated = end < size;
    size_t length = end - start;

    if (length && memchr(data + start, 0, length) != NULL) {
      ref->invalid_data = true;
      return true;
    }

    char *candidate = malloc(length + 1);
    if (!candidate) return false;
    if (length) memcpy(candidate, data + start, length);
    candidate[length] = '\0';
    while (length && candidate[length - 1] == '\r') length--;
    candidate[length] = '\0';
    length = async_strip_ansi(candidate, length);

    bool publish = true;
    if (max_line_length != 0) {
      size_t cap = async_line_limit(max_line_length);
      size_t chars = utf8_character_count(candidate, length);
      if (chars > cap) {
        if (max_line_length > 0) {
          publish = false;
        } else {
          length = utf8_prefix_byte_length(candidate, length, cap);
          candidate[length] = '\0';
        }
      }
    }

    if (publish) {
      if (!session_fuzz_reader_reference_append(ref, candidate)) {
        free(candidate);
        return false;
      }
    } else {
      free(candidate);
    }
    if (!terminated) break;
    start = end + 1;
  }
  return true;
}

/* Exercise the real poll/read producer path as well as the in-process growth
   operations below.  Odd control bytes retain the writer and destroy the
   session with a possibly unterminated line, which forces the cancellation
   pipe to wake the blocked reader.  Even bytes close the producer normally
   and cover newline assembly, ANSI stripping, raw bytes, CR trimming, and
   final unterminated-line publication. */
static void session_fuzz_reader_probe(const uint8_t *data, size_t size) {
  if (!data || size == 0) return;
  AsyncSession *s = session_fuzz_create(data[0]);
  if (!s) return;
  FuzzReaderReference reference = {0};
  bool have_reference = session_fuzz_reader_reference(
      data + 1, size - 1, s->max_line_length, &reference);

  int producer_pipe[2];
  if (pipe(producer_pipe) != 0) {
    session_fuzz_reader_reference_free(&reference);
    async_session_destroy(s);
    return;
  }
  s->fp = fdopen(producer_pipe[0], "r");
  if (!s->fp || !async_make_cancel_pipe(s) ||
      pthread_create(&s->reader, NULL, async_reader, s) != 0) {
    if (!s->fp) close(producer_pipe[0]);
    close(producer_pipe[1]);
    session_fuzz_reader_reference_free(&reference);
    async_session_destroy(s);
    return;
  }
  s->reader_started = true;

  bool wrote = session_fuzz_write_all(producer_pipe[1], data + 1, size - 1);
  if (!wrote || (data[0] & 1)) {
    /* Keep the producer descriptor open on the ordinary cancellation path;
       teardown must not depend on producer EOF.  Wait until the reader has
       consumed any supplied partial line and entered the next poll, so replay
       deterministically covers the cancellation descriptor rather than only
       observing STOP before the blocking call. */
    unsigned target_epoch = size > 1 ? 2 : 1;
    static const struct timespec pause = {.tv_sec = 0, .tv_nsec = 50000};
    for (size_t attempt = 0;
         atomic_load_explicit(&s->test_reader_poll_epoch,
                              memory_order_acquire) < target_epoch &&
         attempt < 20000;
         attempt++)
      nanosleep(&pause, NULL);
    async_session_destroy(s);
    close(producer_pipe[1]);
    session_fuzz_reader_reference_free(&reference);
    return;
  }

  close(producer_pipe[1]);
  pthread_join(s->reader, NULL);
  s->reader_started = false;
  if (!atomic_load_explicit(&s->reader_done, memory_order_acquire))
    abort();
  uint64_t producer_error = atomic_load_explicit(
      &s->producer_error, memory_order_acquire);
  enum AsyncProducerErrorKind error_kind =
      async_unpack_producer_error_kind(producer_error);
  enum AsyncProducerState producer_state =
      atomic_load_explicit(&s->producer_state, memory_order_acquire);
  bool contains_nul = size > 1 && memchr(data + 1, 0, size - 1) != NULL;
  if (contains_nul) {
    if (error_kind != AsyncProducerErrorInvalidData ||
        producer_state != AsyncProducerFailed)
      abort();
  } else if (error_kind == AsyncProducerErrorInvalidData) {
    abort();
  }
  if (have_reference) {
    if (reference.invalid_data != contains_nul ||
        s->count != reference.count)
      abort();
    for (size_t i = 0; i < reference.count; i++) {
      char **block = s->cands_top[i >> CANDS_BLOCK_SHIFT];
      const char *actual = block ? block[i & CANDS_BLOCK_MASK] : NULL;
      if (!actual || strcmp(actual, reference.candidates[i]) != 0) abort();
    }
  }
  session_fuzz_reader_reference_free(&reference);
  async_session_destroy(s);
}

static char *fuzz_copy_string(FuzzInput *input, size_t requested,
                              size_t *out_len) {
  size_t len = requested < fuzz_remaining(input)
                   ? requested
                   : fuzz_remaining(input);
  char *copy = malloc(len + 1);
  if (!copy) return NULL;
  for (size_t i = 0; i < len; i++) {
    uint8_t byte = fuzz_take(input);
    /* The native matcher accepts arbitrary non-NUL bytes.  Producer newlines
       delimit candidates, so map them to spaces in the in-process producer. */
    if (byte == 0) byte = 1;
    if (byte == '\n' || byte == '\r') byte = ' ';
    copy[i] = (char)byte;
  }
  copy[len] = '\0';
  *out_len = len;
  return copy;
}

static bool session_fuzz_append(FuzzSession *fuzz, const char *candidate,
                                size_t len) {
  AsyncSession *s = fuzz->session;
  pthread_mutex_lock(&s->mu);
  size_t count = s->count;
  pthread_mutex_unlock(&s->mu);
  if (count >= SESSION_FUZZ_MAX_CANDIDATES || len == 0) return true;
  return async_append_candidate(s, candidate, len);
}

static void session_fuzz_append_literal(FuzzSession *fuzz, FuzzInput *input) {
  size_t len = fuzz_take(input) & 31;
  char *candidate = fuzz_copy_string(input, len, &len);
  if (!candidate) return;
  session_fuzz_append(fuzz, candidate, len);
  free(candidate);
}

static void session_fuzz_append_run(FuzzSession *fuzz, FuzzInput *input) {
  static const size_t run_sizes[] = {
      1, 2, 31, 127, 2047, 2048, 2049, 4097,
  };
  size_t requested = run_sizes[fuzz_take(input) & 7];
  size_t token_len = 1 + (fuzz_take(input) & 7);
  char *token = fuzz_copy_string(input, token_len, &token_len);
  if (!token) return;
  if (token_len == 0) {
    free(token);
    token = strdup("item");
    token_len = 4;
    if (!token) return;
  }

  char candidate[64];
  for (size_t i = 0; i < requested; i++) {
    pthread_mutex_lock(&fuzz->session->mu);
    size_t count = fuzz->session->count;
    pthread_mutex_unlock(&fuzz->session->mu);
    if (count >= SESSION_FUZZ_MAX_CANDIDATES) break;
    size_t prefix = token_len < sizeof candidate - 4
                        ? token_len
                        : sizeof candidate - 4;
    memcpy(candidate, token, prefix);
    candidate[prefix] = '-';
    candidate[prefix + 1] = (char)('a' + (i % 26));
    candidate[prefix + 2] = (char)('0' + ((i / 26) % 10));
    candidate[prefix + 3] = '\0';
    if (!session_fuzz_append(fuzz, candidate, prefix + 3)) break;
  }
  free(token);
}

static void session_fuzz_append_special(FuzzSession *fuzz,
                                        FuzzInput *input) {
  static const char *const candidates[] = {
      "alpha", "alphabet", "beta", "foo bar", "\xE4\xBD\xA0\xE5\xA5\xBD",
      ("\xE2\x84\xAA" "elvin"), "\xF0\x9F\x9A\x80 launch", "!literal",
  };
  const char *candidate = candidates[fuzz_take(input) & 7];
  session_fuzz_append(fuzz, candidate, strlen(candidate));
}

static uint64_t session_fuzz_submit(FuzzSession *fuzz, FuzzInput *input) {
  uint8_t settings = fuzz_take(input);
  size_t query_len = fuzz_take(input) & 63;
  char *query = fuzz_copy_string(input, query_len, &query_len);
  if (!query) return 0;

  fzf_case_types case_mode = (fzf_case_types)(settings % 3);
  bool fuzzy = (settings & 4) != 0;
  static const size_t filter_only_lengths[] = {0, 1, 3, 8};
  size_t fo_length = filter_only_lengths[(settings >> 3) & 3];
  bool fo_logic_and = (settings & 32) != 0;

  pthread_mutex_lock(&fuzz->session->mu);
  size_t count = fuzz->session->count;
  pthread_mutex_unlock(&fuzz->session->mu);
  size_t limit;
  switch (settings >> 6) {
  case 0: limit = 0; break;
  case 1: limit = 1; break;
  case 2: limit = 10; break;
  default: limit = count > 1 ? count / 2 : 1; break;
  }

  uint64_t request_id = async_submit_request_resolved(
      fuzz->session, query, query_len, limit, case_mode, fuzzy, fo_length,
      fo_logic_and);
  if (request_id) fuzz->submitted = true;
  return request_id;
}

static void session_fuzz_check_invariants(FuzzSession *fuzz) {
  AsyncSession *s = fuzz->session;
  uint64_t next_id, latest_id, queued_id, current_id;
  bool queued_filter, current_filter, latest_filter;

  pthread_mutex_lock(&s->score_req_mu);
  next_id = s->score_next_id;
  latest_id = s->score_latest_id;
  queued_id = s->score_req_id;
  current_id = s->score_current_id;
  queued_filter = s->score_req_filter != NULL;
  current_filter = s->score_current_filter != NULL;
  latest_filter = s->score_latest_filter != NULL;
  size_t completed = atomic_load_explicit(
      &s->score_progress_completed, memory_order_relaxed);
  size_t total = atomic_load_explicit(
      &s->score_progress_total, memory_order_relaxed);
  pthread_mutex_unlock(&s->score_req_mu);

  if (latest_id > next_id || queued_id > next_id || current_id > next_id)
    session_fuzz_fail(fuzz, "a request ID is in the future");
  if ((queued_id != 0) != queued_filter)
    session_fuzz_fail(fuzz, "queued request ownership is inconsistent");
  if ((current_id != 0) != current_filter)
    session_fuzz_fail(fuzz, "running request ownership is inconsistent");
  if ((latest_id != 0) != latest_filter)
    session_fuzz_fail(fuzz, "latest request ownership is inconsistent");
  if (queued_id && current_id && queued_id <= current_id)
    session_fuzz_fail(fuzz, "a queued replacement does not follow running work");

  pthread_mutex_lock(&s->score_res_mu);
  uint64_t result_id = s->score_result_id;
  uint64_t error_id = s->score_error_id;
  uint64_t snapshot_generation = s->score_snapshot_generation;
  size_t result_count = s->score_count;
  size_t result_limit = s->score_result_limit;
  size_t result_pool = s->score_result_pool_gen;
  bool result_filter = s->score_result_filter != NULL;
  bool results_present = s->score_results != NULL;
  for (size_t i = 0; i < result_count; i++) {
    if (s->score_results[i].score <= 0)
      session_fuzz_fail(fuzz, "a published candidate has a non-positive score");
    if (i > 0 && s->score_results[i - 1].score < s->score_results[i].score)
      session_fuzz_fail(fuzz, "published candidates are not score-sorted");
    if (s->score_results[i].idx >= result_pool)
      session_fuzz_fail(fuzz, "a published candidate index exceeds its pool");
  }
  pthread_mutex_unlock(&s->score_res_mu);

  pthread_mutex_lock(&s->mu);
  size_t pool = s->count;
  pthread_mutex_unlock(&s->mu);
  if (result_id > next_id || error_id > next_id)
    session_fuzz_fail(fuzz, "a published ID is in the future");
  if ((result_id != 0) != result_filter)
    session_fuzz_fail(fuzz, "result request ownership is inconsistent");
  if ((result_count != 0) && !results_present)
    session_fuzz_fail(fuzz, "a non-empty result has no storage");
  if (result_limit && result_count > result_limit)
    session_fuzz_fail(fuzz, "a result exceeds its requested limit");
  if (result_pool > pool)
    session_fuzz_fail(fuzz, "a result refers to a future pool boundary");
  if (result_id < fuzz->observed_result_id)
    session_fuzz_fail(fuzz, "an obsolete result replaced a newer result");
  if (snapshot_generation < fuzz->observed_snapshot_generation)
    session_fuzz_fail(fuzz, "snapshot generation moved backwards");
  fuzz->observed_result_id = result_id;
  fuzz->observed_snapshot_generation = snapshot_generation;

  if (completed > total)
    session_fuzz_fail(fuzz, "request progress exceeds its total");

  pthread_mutex_lock(&s->batch_cache.mu);
  BatchCache *batch_cache = &s->batch_cache;
  size_t query_lru_count = 0;
  size_t active_query_count = 0;
  BatchQuery *query_previous = NULL;
  for (BatchQuery *query = batch_cache->query_head; query;
       query = query->lru_next) {
    if (query->lru_prev != query_previous)
      session_fuzz_fail(fuzz, "batch query LRU links disagree");
    if (batch_cache_find_query_locked(
            batch_cache, query->query, query->case_mode,
            query->fuzzy, query->hash) != query)
      session_fuzz_fail(fuzz, "batch query hash lost an LRU record");
    size_t owner_entry_count = 0;
    BatchCacheEntry *owner_previous = NULL;
    for (BatchCacheEntry *entry = query->entry_head; entry;
         entry = entry->owner_next) {
      if (entry->owner != query || entry->owner_prev != owner_previous)
        session_fuzz_fail(fuzz, "batch entry owner links disagree");
      owner_previous = entry;
      owner_entry_count++;
    }
    if (owner_previous != query->entry_tail ||
        owner_entry_count != query->entry_count)
      session_fuzz_fail(fuzz, "batch query entry count disagrees");
    if (query->external_refs) active_query_count++;
    query_previous = query;
    query_lru_count++;
  }
  if (query_previous != batch_cache->query_tail ||
      query_lru_count != batch_cache->query_count)
    session_fuzz_fail(fuzz, "batch query LRU count disagrees");

  size_t query_hash_count = 0;
  for (size_t bucket = 0; bucket < batch_cache->query_bucket_count; bucket++)
    for (BatchQuery *query = batch_cache->query_buckets[bucket]; query;
         query = query->hash_next) {
      if (batch_cache_query_bucket(batch_cache, query->hash) != bucket)
        session_fuzz_fail(fuzz, "batch query is in the wrong hash bucket");
      query_hash_count++;
    }
  if (query_hash_count != batch_cache->query_count)
    session_fuzz_fail(fuzz, "batch query hash count disagrees");

  size_t entry_lru_count = 0;
  BatchCacheEntry *entry_previous = NULL;
  for (BatchCacheEntry *entry = batch_cache->head; entry;
       entry = entry->lru_next) {
    if (entry->lru_prev != entry_previous)
      session_fuzz_fail(fuzz, "batch entry LRU links disagree");
    if (batch_cache_find_entry_locked(
            batch_cache, entry->owner, entry->batch_id) != entry)
      session_fuzz_fail(fuzz, "batch entry hash lost an LRU record");
    entry_previous = entry;
    entry_lru_count++;
  }
  if (entry_previous != batch_cache->tail ||
      entry_lru_count != batch_cache->entry_count)
    session_fuzz_fail(fuzz, "batch entry LRU count disagrees");
  if (batch_cache->used_bytes > batch_cache->max_bytes)
    session_fuzz_fail(fuzz, "batch cache exceeds its byte budget");
  if (batch_cache->query_count > batch_cache->max_queries &&
      batch_cache->query_count - batch_cache->max_queries >
          active_query_count)
    session_fuzz_fail(fuzz, "inactive batch queries exceed the count cap");
  pthread_mutex_unlock(&s->batch_cache.mu);

  enum AsyncRequestState latest_state = async_request_state(
      latest_id, latest_id, queued_id, current_id, result_id, error_id);
  if (latest_id && latest_state == AsyncRequestUnknown)
    session_fuzz_fail(fuzz, "the latest request has an unknown state");

  /* Exercise the same owned-copy boundary used by the Emacs candidate and
     snapshot APIs.  A concurrent publication may make this copy newer than
     the metadata above, so validate it independently. */
  size_t copied_count = 0, copied_limit = 0;
  size_t copied_completed = 0, copied_total = 0;
  size_t copied_filtered = 0, copied_source_total = 0;
  uint64_t copied_generation = 0, copied_error_id = 0;
  AsyncResultObservation copied_result = {0};
  char *copied_filter = NULL, *copied_error = NULL;
  fzf_case_types copied_case_mode = CaseSmart;
  bool copied_fuzzy = true, copied_filter_only = false;
  bool copied_allocation_failed = false;
  ScoredStr *copied = async_copy_public_result(
      s, true, &copied_count, &copied_result, &copied_filter, &copied_limit,
      &copied_case_mode, &copied_fuzzy, &copied_filter_only,
      &copied_generation, &copied_completed, &copied_total,
      &copied_error_id, &copied_error, &copied_filtered,
      &copied_source_total, &copied_allocation_failed);
  if (copied_allocation_failed)
    session_fuzz_fail(fuzz, "an owned result copy allocation failed");
  if (copied_limit && copied_count > copied_limit)
    session_fuzz_fail(fuzz, "an owned result copy exceeds its limit");
  if (copied_completed > copied_total)
    session_fuzz_fail(fuzz, "owned result progress exceeds its total");
  if (copied_result.request_id && !copied_filter)
    session_fuzz_fail(fuzz, "an owned result copy lost its request query");
  for (size_t i = 0; i < copied_count; i++)
    if (copied[i].idx >= copied_result.pool_generation)
      session_fuzz_fail(fuzz, "an owned result copy has an invalid index");
  (void)copied_case_mode;
  (void)copied_fuzzy;
  (void)copied_filter_only;
  (void)copied_generation;
  (void)copied_error_id;
  (void)copied_filtered;
  (void)copied_source_total;
  async_free_public_result(copied, copied_count);
  free(copied_filter);
  free(copied_error);
}

static bool session_fuzz_is_quiescent(AsyncSession *s) {
  pthread_mutex_lock(&s->mu);
  size_t pool = s->count;
  pthread_mutex_unlock(&s->mu);

  pthread_mutex_lock(&s->score_req_mu);
  uint64_t latest_id = s->score_latest_id;
  bool request_idle = s->score_req_id == 0 && s->score_current_id == 0;
  pthread_mutex_unlock(&s->score_req_mu);

  pthread_mutex_lock(&s->score_res_mu);
  bool terminal = latest_id == 0 ||
      (s->score_error_id == latest_id) ||
      (s->score_result_id == latest_id && s->score_result_pool_gen == pool);
  pthread_mutex_unlock(&s->score_res_mu);
  return request_idle && terminal &&
      !atomic_load_explicit(&s->score_growth_pending, memory_order_acquire);
}

static bool session_fuzz_wait_quiescent(FuzzSession *fuzz) {
  static const struct timespec pause = {.tv_sec = 0, .tv_nsec = 50000};
  for (size_t attempt = 0; attempt < 20000; attempt++) {
    if (session_fuzz_is_quiescent(fuzz->session)) return true;
    if ((attempt & 31) == 0) session_fuzz_check_invariants(fuzz);
    nanosleep(&pause, NULL);
  }
  return false;
}

static void session_fuzz_wait_running(FuzzSession *fuzz) {
  static const struct timespec pause = {.tv_sec = 0, .tv_nsec = 50000};
  for (size_t attempt = 0; attempt < 2000; attempt++) {
    pthread_mutex_lock(&fuzz->session->score_req_mu);
    bool running = fuzz->session->score_current_id != 0;
    bool idle = fuzz->session->score_req_id == 0;
    pthread_mutex_unlock(&fuzz->session->score_req_mu);
    if (running || idle) return;
    nanosleep(&pause, NULL);
  }
}

static int session_fuzz_reference_cmp(const void *left, const void *right) {
  const ScoredStr *a = left;
  const ScoredStr *b = right;
  if (a->score != b->score) return a->score > b->score ? -1 : 1;
  if (a->idx != b->idx) return a->idx < b->idx ? -1 : 1;
  return 0;
}

static void session_fuzz_check_reference(FuzzSession *fuzz) {
  AsyncSession *s = fuzz->session;
  pthread_mutex_lock(&s->score_req_mu);
  uint64_t latest_request = s->score_latest_id;
  pthread_mutex_unlock(&s->score_req_mu);
  pthread_mutex_lock(&s->score_res_mu);
  uint64_t latest_result = s->score_result_id;
  uint64_t latest_error = s->score_error_id;
  size_t actual_count = s->score_count;
  size_t pool = s->score_result_pool_gen;
  size_t limit = s->score_result_limit;
  fzf_case_types case_mode = s->score_result_case_mode;
  bool fuzzy = s->score_result_fuzzy;
  bool filter_only = s->score_result_filter_only;
  char *query = s->score_result_filter ? strdup(s->score_result_filter) : NULL;
  ScoredStr *actual = actual_count ? malloc(actual_count * sizeof *actual) : NULL;
  if (actual) memcpy(actual, s->score_results, actual_count * sizeof *actual);
  pthread_mutex_unlock(&s->score_res_mu);

  if (latest_error == latest_request) {
    free(actual);
    free(query);
    return;
  }
  if (latest_result == 0 || !query || (actual_count && !actual)) {
    free(actual);
    free(query);
    session_fuzz_fail(fuzz, "the latest request has no comparable result");
  }

  ScoredStr *reference = pool ? malloc(pool * sizeof *reference) : NULL;
  if (pool && !reference) {
    free(actual);
    free(query);
    return;
  }
  char *pattern_query = *query ? strdup(query) : NULL;
  fzf_pattern_t *pattern = pattern_query
                               ? fzf_parse_pattern(
                                     case_mode, false, pattern_query, fuzzy)
                               : NULL;
  fzf_slab_t *slab = fzf_make_default_slab();
  size_t matched = 0;

  pthread_mutex_lock(&s->mu);
  for (size_t i = 0; i < pool; i++) {
    const char *candidate =
        s->cands_top[i >> CANDS_BLOCK_SHIFT][i & CANDS_BLOCK_MASK];
    int score;
    if (!pattern)
      score = 1;
    else if (filter_only)
      score = fzf_has_match(candidate, pattern, slab) ? 1 : 0;
    else
      score = fzf_get_score(candidate, pattern, slab);
    if (score > 0)
      reference[matched++] = (ScoredStr){
          .str = (char *)candidate, .score = score, .idx = (uint32_t)i};
  }
  pthread_mutex_unlock(&s->mu);

  size_t emit = limit && limit < matched ? limit : matched;
  if (filter_only && pattern && emit > 1) {
    for (size_t i = 0; i < emit; i++)
      reference[i].score = fzf_get_score(reference[i].str, pattern, slab);
    qsort(reference, emit, sizeof *reference, session_fuzz_reference_cmp);
  } else if (!filter_only && matched > 1) {
    qsort(reference, matched, sizeof *reference, session_fuzz_reference_cmp);
  }

  if (actual_count != emit)
    session_fuzz_fail(fuzz, "interactive and batch result counts differ");
  for (size_t i = 0; i < emit; i++) {
    if (actual[i].idx != reference[i].idx ||
        actual[i].score != reference[i].score ||
        strcmp(actual[i].str, reference[i].str) != 0) {
      fprintf(stderr,
              "session differential: query='%s' pool=%zu limit=%zu "
              "filter_only=%d position=%zu actual={idx=%u score=%d str='%s'} "
              "reference={idx=%u score=%d str='%s'}\n",
              query, pool, limit, (int)filter_only, i,
              actual[i].idx, actual[i].score, actual[i].str,
              reference[i].idx, reference[i].score, reference[i].str);
      session_fuzz_fail(fuzz, "interactive and batch ordered results differ");
    }
  }

  if (pattern) fzf_free_pattern(pattern);
  if (slab) fzf_free_slab(slab);
  free(pattern_query);
  free(reference);
  free(actual);
  free(query);
}

static void session_fuzz_verify(FuzzSession *fuzz) {
  if (!fuzz->submitted) return;
  if (!session_fuzz_wait_quiescent(fuzz))
    session_fuzz_fail(fuzz, "the interactive engine did not quiesce");
  session_fuzz_check_invariants(fuzz);
  session_fuzz_check_reference(fuzz);
}

static void session_fuzz_run(const uint8_t *data, size_t size) {
  if (!data || size == 0 || size > SESSION_FUZZ_MAX_INPUT) return;
  FuzzInput input = {.data = data, .size = size, .offset = 0};
  AsyncSession *session = session_fuzz_create(fuzz_take(&input));
  if (!session) return;
  FuzzSession fuzz = {.session = session};
  bool stop_while_active = false;

  while (input.offset < input.size &&
         fuzz.operation < SESSION_FUZZ_MAX_OPERATIONS) {
    uint8_t control = fuzz_take(&input);
    switch (control & 7) {
    case 0:
      session_fuzz_append_literal(&fuzz, &input);
      break;
    case 1:
      session_fuzz_append_run(&fuzz, &input);
      break;
    case 2:
      session_fuzz_submit(&fuzz, &input);
      break;
    case 3:
      session_fuzz_check_invariants(&fuzz);
      break;
    case 4: {
      uint8_t mode = fuzz_take(&input);
      if ((mode & 3) == 0)
        session_fuzz_wait_running(&fuzz);
      else
        for (unsigned i = 0; i < 1 + (mode & 31); i++) sched_yield();
      break;
    }
    case 5:
      session_fuzz_verify(&fuzz);
      break;
    case 6:
      session_fuzz_append_special(&fuzz, &input);
      break;
    case 7:
      /* A high-bit stop after useful work exercises teardown with producer or
         scorer activity still live.  Other opcode-7 bytes are cheap polls. */
      if ((control & 0x80) && fuzz.submitted) stop_while_active = true;
      else session_fuzz_check_invariants(&fuzz);
      break;
    }
    fuzz.operation++;
    if (stop_while_active) break;
    session_fuzz_check_invariants(&fuzz);
  }

  if (!stop_while_active) {
    atomic_store_explicit(&session->reader_done, true, memory_order_release);
    atomic_store_explicit(
        &session->producer_state, AsyncProducerComplete, memory_order_release);
    session_fuzz_verify(&fuzz);
  }
  async_session_destroy(session);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  session_fuzz_check_candidate_analyzer(data, size);
  if (size <= SESSION_FUZZ_MAX_INPUT)
    session_fuzz_reader_probe(data, size);
  session_fuzz_run(data, size);
  return 0;
}

#ifdef FZF_SESSION_FUZZ_STANDALONE
static int session_fuzz_replay_file(const char *path) {
  FILE *file = fopen(path, "rb");
  if (!file) return 1;
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    return 1;
  }
  long length = ftell(file);
  if (length < 0 || length > SESSION_FUZZ_MAX_INPUT) {
    fclose(file);
    return 1;
  }
  rewind(file);
  uint8_t *bytes = malloc((size_t)length + 1);
  if (!bytes) {
    fclose(file);
    return 1;
  }
  size_t got = fread(bytes, 1, (size_t)length, file);
  fclose(file);
  if (got != (size_t)length) {
    free(bytes);
    return 1;
  }
  LLVMFuzzerTestOneInput(bytes, got);
  free(bytes);
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 2) return 2;
  for (int i = 1; i < argc; i++)
    if (session_fuzz_replay_file(argv[i]) != 0) return 1;
  printf("Replayed %d interactive session corpus files.\n", argc - 1);
  return 0;
}
#endif
