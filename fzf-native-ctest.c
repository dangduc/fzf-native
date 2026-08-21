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

#define FZF_NATIVE_CTEST 1
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

/* The `cap` parameter is ignored under the chunked-storage design — the
   reader allocates blocks lazily.  Kept in the signature so existing test
   sites remain readable without rewrites. */
static AsyncSession *make_async_session(FILE *fp, size_t cap) {
  (void)cap;
  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  s->fp = fp;
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  return s;
}

static void free_async_session(AsyncSession *s) {
  arena_free(&s->arena);
  for (size_t k = 0; k < CANDS_TOP_CAP; k++)
    if (s->cands_top[k]) free(s->cands_top[k]);
  pthread_cond_destroy(&s->score_req_cond);
  pthread_mutex_destroy(&s->score_req_mu);
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
  size_t budget = sizeof(BatchQuery) + strlen("foo") + 1 +
                  sizeof(BatchCacheEntry) + sizeof(uint16_t);
  BatchCache cache;
  batch_cache_init(&cache, budget);
  BatchQuery *query = batch_cache_acquire_query(
      &cache, "foo", CaseSmart, true);
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
  CHECK(cache.used_bytes <= cache.max_bytes);

  batch_cache_release_query(&cache, query);
  batch_cache_free(&cache);
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
  async_score_batches(&shared, slab);
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
  async_score_batches(&shared, slab);
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
  CHECK(async_request_state(4, 5, 5, 0, 3, 0) == AsyncRequestCancelled);
  CHECK(async_request_state(6, 5, 0, 0, 5, 0) == AsyncRequestUnknown);
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

/* =====================================================================
 * async_session_destroy_async (off-main detached teardown)
 * ===================================================================== */

/* Reader stub: park until s->stop is signaled, then return.  Acts as a
   stand-in for the real `async_reader' so the destroy path has a
   join-able thread without us having to fork a subprocess. */
static void *test_destroy_reader_stub(void *arg) {
  AsyncSession *s = arg;
  while (!atomic_load_explicit(&s->stop, memory_order_relaxed))
    usleep(1000);   /* 1 ms */
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
  pthread_mutex_init(&s->mu, NULL);
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

static double monotonic_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
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
    usleep(5000);   /* 5 ms */
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
    usleep(5000);
  }
  CHECK(atomic_load_explicit(&async_destroy_completions,
                             memory_order_relaxed) - base == (uint64_t)N);
}

/* ================================================================= */

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

  printf("--- async_strip_ansi ---\n");
  RUN(test_strip_ansi_no_escape);
  RUN(test_strip_ansi_simple_color);
  RUN(test_strip_ansi_multiple_sequences);
  RUN(test_strip_ansi_bare_esc);

  printf("--- async_reader ---\n");
  RUN(test_async_reader_basic);
  RUN(test_async_reader_coalesces_growth_after_request);
  RUN(test_async_reader_ansi_stripping);
  RUN(test_async_reader_many_lines);
  RUN(test_async_reader_long_line);

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
  RUN(test_cache_pool_gen_distinguishes_stale);
  RUN(test_cache_exact_separates_case_and_fuzzy_modes);
  RUN(test_cache_exact_requires_sufficient_result_capacity);
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
  RUN(test_async_request_state_precedence);
  RUN(test_matcher_failure_retains_last_completed_result);
  RUN(test_async_request_identity_includes_matching_options);
  RUN(test_async_running_request_reuse_requires_latest_slot);
  RUN(test_async_worker_pool_reuses_threads_across_rounds);

  printf("--- async_session_destroy_async ---\n");
  RUN(test_destroy_async_handles_null);
  RUN(test_destroy_async_returns_fast);
  RUN(test_destroy_async_many_in_flight);

  if (failed == 0) {
    printf("\nAll tests passed.\n");
    return 0;
  } else {
    printf("\n%d check(s) failed.\n", failed);
    return 1;
  }
}
