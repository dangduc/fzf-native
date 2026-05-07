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
  for (size_t i = 0; i < s->count; i++) free(s->cands[i]);
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

  if (failed == 0) {
    printf("\nAll tests passed.\n");
    return 0;
  } else {
    printf("\n%d check(s) failed.\n", failed);
    return 1;
  }
}
