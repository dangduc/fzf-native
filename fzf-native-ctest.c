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

#include "fzf-native-module.c"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

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

static void test_small_n_qsort_fallback(void) {
  /* n=8 hits the n < 64 qsort fallback. Verify it still sorts descending. */
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

static AsyncSession *make_async_session(FILE *fp, size_t cap) {
  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  s->fp    = fp;
  s->cap   = cap;
  s->cands = calloc(cap, sizeof *s->cands);
  if (!s->cands) { free(s); return NULL; }
  pthread_mutex_init(&s->mu, NULL);
  return s;
}

static void free_async_session(AsyncSession *s) {
  arena_free(&s->arena);
  free(s->cands);
  pthread_mutex_destroy(&s->mu);
  free(s);
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
  CHECK(strcmp(s->cands[0], "alpha") == 0);
  CHECK(strcmp(s->cands[1], "beta")  == 0);
  CHECK(strcmp(s->cands[2], "gamma") == 0);
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
  CHECK(strcmp(s->cands[0], "file.txt") == 0);
  CHECK(strcmp(s->cands[1], "plain.c")  == 0);
  free_async_session(s);
}

static void test_async_reader_buffer_growth(void) {
  /* Initial cap=4, write 32 lines — exercises the realloc doubling path. */
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
    CHECK(strcmp(s->cands[i], expected) == 0);
  }
  free_async_session(s);
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

  cache_insert(&c, "fo", 1000, top, 2, NULL, 0);

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
  cache_insert(&c, "fo", 100, top, 1, NULL, 0);

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

  cache_insert(&c, "fo", 100, v1, 1, NULL, 0);
  cache_insert(&c, "fo", 200, v2, 2, NULL, 0);
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
    cache_insert(&c, qbuf, (size_t)i, one, 1, NULL, 0);
  }
  CHECK(c.count == MAX);

  /* Insert one more — should evict q0 (the LRU tail). */
  cache_insert(&c, "extra", 999, one, 1, NULL, 0);
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
    cache_insert(&c, qbuf, (size_t)i, one, 1, NULL, 0);
  }

  /* Touch q0 — moves it to head (MRU). */
  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "q0", &out, &out_count, &out_sidx, &out_gen) == true);
  free(out);

  /* Now the LRU tail is q1.  Insert one more; q1 should be evicted. */
  cache_insert(&c, "extra", 999, one, 1, NULL, 0);

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
  cache_insert(&c, "nothing", 500, NULL, 0, NULL, 0);

  ScoredStr *out = (ScoredStr *)0xdeadbeef;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 99, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "nothing", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out == NULL);
  CHECK(out_count == 0);
  CHECK(out_gen == 500);
  cache_free(&c);
}

static void test_cache_pool_gen_distinguishes_stale(void) {
  /* The cache itself doesn't decide fresh-vs-stale — that's the dispatch
     layer's job — but it must faithfully report pool_gen so the dispatch
     can compare it against the current pool size. */
  Cache c;
  cache_init(&c, 20);
  ScoredStr top[1] = { make_top("alpha", 1) };
  cache_insert(&c, "fo", 100, top, 1, NULL, 0);

  ScoredStr *out = NULL;
  SharedIdx *out_sidx = NULL;
  size_t out_count = 0, out_gen = 0;
  CHECK(cache_lookup_exact(&c, "fo", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 100);
  free(out);

  /* Re-insert at a new pool_gen; lookup should reflect the latest. */
  cache_insert(&c, "fo", 5000, top, 1, NULL, 0);
  out = NULL; out_count = 0; out_gen = 0;
  CHECK(cache_lookup_exact(&c, "fo", &out, &out_count, &out_sidx, &out_gen) == true);
  CHECK(out_gen == 5000);
  free(out);
  cache_free(&c);
}

/* ================================================================= */

int main(void) {
  printf("--- counting_sort_candidates ---\n");
  RUN(test_n_zero);
  RUN(test_n_one);
  RUN(test_small_n_qsort_fallback);
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

  printf("--- async_strip_ansi ---\n");
  RUN(test_strip_ansi_no_escape);
  RUN(test_strip_ansi_simple_color);
  RUN(test_strip_ansi_multiple_sequences);
  RUN(test_strip_ansi_bare_esc);

  printf("--- async_reader ---\n");
  RUN(test_async_reader_basic);
  RUN(test_async_reader_ansi_stripping);
  RUN(test_async_reader_buffer_growth);

  printf("--- cache (phase 1: exact-match) ---\n");
  RUN(test_cache_lookup_miss_on_empty);
  RUN(test_cache_insert_then_lookup_hit);
  RUN(test_cache_lookup_miss_distinct_query);
  RUN(test_cache_insert_updates_in_place);
  RUN(test_cache_lru_eviction_at_capacity);
  RUN(test_cache_touch_on_hit);
  RUN(test_cache_insert_zero_count);
  RUN(test_cache_pool_gen_distinguishes_stale);

  if (failed == 0) {
    printf("\nAll tests passed.\n");
    return 0;
  } else {
    printf("\n%d check(s) failed.\n", failed);
    return 1;
  }
}
