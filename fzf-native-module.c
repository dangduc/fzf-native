/* strdup is POSIX (not C11); Linux glibc hides it under c11 without this */
#if defined(__linux__) && !defined(_POSIX_C_SOURCE)
#  define _POSIX_C_SOURCE 200809L
#endif
#include <ctype.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "emacs-module.h"
#include "fzf.h"
#include <stdio.h>
#include <stdarg.h>

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
#include <stdatomic.h>
#include <pthread.h>
// for sysconf(_SC_NPROCESSORS_ONLN);
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <fcntl.h>
#endif

#ifdef _WIN32
#  define EXPORT __declspec(dllexport)
#else
#  define EXPORT
#endif

/* Compile-time logging gate. Build with FZF_NATIVE_DEBUG=1 to enable
   file logging; otherwise fzf_log() is a no-op macro and all call-site
   args are discarded by the preprocessor (zero runtime cost). */
#ifdef FZF_NATIVE_DEBUG
static FILE *fzf_log_file = NULL;
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
static pthread_mutex_t fzf_log_mu = PTHREAD_MUTEX_INITIALIZER;
#endif

static void fzf_log(const char *format, ...) {
  if (!fzf_log_file) return;

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  pthread_mutex_lock(&fzf_log_mu);
#endif

  time_t now = time(NULL);
  struct tm *t = localtime(&now);
  char tstr[64];
  strftime(tstr, sizeof(tstr), "%Y-%m-%d %H:%M:%S", t);

  fprintf(fzf_log_file, "[%s] ", tstr);
  va_list args;
  va_start(args, format);
  vfprintf(fzf_log_file, format, args);
  va_end(args);
  fflush(fzf_log_file);

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  pthread_mutex_unlock(&fzf_log_mu);
#endif
}
#else
#define fzf_log(...) ((void)0)
#endif

static struct emacs_runtime *global_rt;

/** See https://wambold.com/Martin/writings/alignof.html */
#define ALIGNOF(type) offsetof (struct { char c; type member; }, member)

/** MSVC does not recognize __attribute__((unused)), so define it away. */
#ifdef _MSC_VER
#define UNUSED(x) x
#else
#define UNUSED(x) __attribute__((unused)) x
#endif

#ifdef _WIN32
typedef long ssize_t;
#endif

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define BATCH_SIZE 2048

EXPORT
int plugin_is_GPL_compatible;

emacs_value Qnil, Qlistofzero, Fcons, Flist, Qt;
emacs_value Fhashtablep, Fmessage, Fvectorp, Fconsp, Fcdr, Fcar, Fvconcat;
emacs_value Ffunctionp, Fsymbolp, Fsymbolname, Flength, Fnth, Fprinc, Freverse;
emacs_value Qcompletion_score, Fput_text_property, Qzero, Qone;
emacs_value Fencode_coding_string, Qutf_8;


/** An Emacs string made accessible by copying. */
struct Str { char *b; size_t len; };

/** Module userdata that gets allocated once at initialization. */
struct Data {
  unsigned max_workers;
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  pthread_t threads[];
#endif
};

/** Intrusive linked list of bump allocation blocks. */
struct Bump {
  struct Bump *next;
  char *cursor, *limit, b[];
};

static void bump_free(struct Bump *head) {
  while (head) {
    struct Bump *next = head->next;
    free(head);
    head = next;
  }
}


// Copied from https://github.com/axelf4/hotfuzz
/** Copies the Emacs string to make its contents accessible. */
static struct Str copy_valid_emacs_string(emacs_env *env, struct Bump **bump, emacs_value value) {
  char *buf = NULL;
  ptrdiff_t origlen, len;
  if (*bump) {
    // Opportunistically try to copy into remaining space
    buf = (*bump)->cursor;
    len = origlen = (*bump)->limit - (*bump)->cursor;
  }
  // Determine the size of the string (including null-terminator)
  if (env->copy_string_contents(env, value, buf, &len)) {
    if (buf) goto success;
  } else {
    if (!buf || len == origlen) return (struct Str) { 0 };
    env->non_local_exit_clear(env);
  }

  size_t capacity = *bump ? 2 * ((*bump)->limit - (*bump)->b) : 2048;
  if (capacity < (size_t) len) capacity = len + alignof(uint64_t) - 1;
  struct Bump *new;
  if (!(new = malloc(sizeof *new + capacity))) return (struct Str) { 0 };
  *new = (struct Bump) { .next = *bump, .cursor = new->b, .limit = new->b + capacity };
  *bump = new;

  env->copy_string_contents(env, value, buf = new->cursor, &len);
success:
  (*bump)->cursor = (char *) (((uintptr_t) (*bump)->cursor + len
                               + alignof(uint64_t) - 1) & ~(alignof(uint64_t) - 1));
  return (struct Str) { buf, len - 1 };
}

/**
 * Like copy_emacs_string, but if the direct copy fails (e.g. because VALUE is
 * an invalid unibyte string that Emacs's module API refuses to hand out via
 * copy_string_contents, signaling `unicode-string-p'), fall back to encoding
 * VALUE through `encode-coding-string' with UTF-8 and retry. This lets us
 * accept arbitrary multibyte and byte-junk candidates without aborting the
 * whole batch. The original VALUE is never mutated; the encoding happens on a
 * fresh Emacs string that we then copy into the bump. Returns a zero Str if
 * even the coerced copy fails, in which case callers should skip the
 * candidate.
 */
static struct Str copy_emacs_string(emacs_env *env, struct Bump **bump,
                                            emacs_value value) {
  struct Str s = copy_valid_emacs_string(env, bump, value);
  if (s.b) return s;

  /* copy_string_contents signaled (likely unicode-string-p). Clear the
     pending non-local exit and try to coerce the string through
     encode-coding-string, which handles the raw-byte case. */
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
  }

  emacs_value encode_args[] = { value, Qutf_8, Qt };
  emacs_value encoded = env->funcall(env, Fencode_coding_string, 3, encode_args);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return (struct Str) { 0 };
  }

  s = copy_emacs_string(env, bump, encoded);
  if (!s.b && env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
  }
  return s;
}

struct Candidate {
  emacs_value value;
  struct Str s;
  int score;
};

static int cmp_candidate(const void *a, const void *b) {
  // This way to get fzf sorted correctly with qsort.
  return ((struct Candidate *) b)->score - ((struct Candidate *) a)->score;
  /* return ((struct Candidate *) a)->score - ((struct Candidate *) b)->score; */
}

/* Counting sort of xs[0..n-1] by score, descending.
   O(n + max_score). Falls back to qsort if allocations fail.
   Stable: same-score candidates keep input order.
   Caller must ensure every xs[i].score >= 0; negative scores would
   index count[] out of bounds (undefined behavior). */
static void counting_sort_candidates(struct Candidate *xs, size_t n) {
  if (n <= 1) return;
  /* For tiny inputs, qsort beats counting sort because the malloc/calloc
     round-trip dominates. Threshold chosen empirically; below ~64 the two
     are within noise on M-series and recent x86. */
  if (n < 64) { qsort(xs, n, sizeof *xs, cmp_candidate); return; }
  int max_score = 0;
  for (size_t i = 0; i < n; i++)
    if (xs[i].score > max_score) max_score = xs[i].score;

  int *count = calloc((size_t)(max_score + 1), sizeof *count);
  if (!count) { qsort(xs, n, sizeof *xs, cmp_candidate); return; }

  for (size_t i = 0; i < n; i++) count[xs[i].score]++;

  /* Convert counts to start positions for descending order. */
  int pos = 0;
  for (int s = max_score; s >= 0; s--) { int c = count[s]; count[s] = pos; pos += c; }

  struct Candidate *out = malloc(n * sizeof *out);
  if (!out) { free(count); qsort(xs, n, sizeof *xs, cmp_candidate); return; }

  for (size_t i = 0; i < n; i++) out[count[xs[i].score]++] = xs[i];
  memcpy(xs, out, n * sizeof *xs);
  free(out);
  free(count);
}

struct Batch {
  unsigned len;
  struct Candidate xs[BATCH_SIZE];
};

struct Shared {
  fzf_pattern_t *pattern;
  struct Batch *const batches;
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  _Atomic ssize_t remaining;
#else
  ssize_t remaining;
#endif
};

// Most of the threading lifted from https://github.com/axelf4/hotfuzz
static void *worker_routine(void *ptr) {
  /* printf("-----\nStarting Worker Routine\n-----\n"); */
  // Create a one-time use slab.
  fzf_slab_t *slab = fzf_make_default_slab();

  struct Shared *shared = ptr;
  fzf_pattern_t *pattern = shared->pattern;
  ssize_t batch_idx;

#ifdef _WIN32
  while ((batch_idx = --shared->remaining) >= 0) {
#endif
  // Atomic fetch-and-decrement for shared->remaining
  // --shared->remaining would return the decremented value whereas
  // atomic_fetch_sub_explicit returns the original value before decrement.
  // So, use batch_idx - 1 when handling the idx.
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  while ((batch_idx = atomic_fetch_sub_explicit(&shared->remaining,
                                                1,
                                                memory_order_seq_cst) - 1) >= 0) {
#endif
    struct Batch *batch = shared->batches + batch_idx;
    unsigned n = 0;

    if (pattern) {
      for (unsigned i = 0; i < batch->len; ++i) {
        struct Candidate x = batch->xs[i];
        /* You can get the score/position for as many items as you want */
        int score = fzf_get_score(x.s.b, pattern, slab);
        if (score > 0) {
          /* printf("Str: %s # = %d | i = %d, batch->len = %d, batch_idx = %zd\n", */
          /*        x.s.b, score, i, batch->len, batch_idx); */
          x.score = score;
          batch->xs[n++] = x;
        }
      }
    }
    batch->len = n;
  }

  // Free one-time use slab.
  fzf_free_slab(slab);
  /* printf("-----\nEnding Worker Routine\n-----\n"); */
  return NULL;
}

// fzf-native-score-all COLLECTION QUERY &optional SLAB
emacs_value fzf_native_score_all(emacs_env *env,
                                 ptrdiff_t nargs,
                                 emacs_value args[],
                                 void UNUSED(*data_ptr)) {
  struct Data *data = NULL;
  struct Bump *bump = NULL;
  struct Batch *batches = NULL;
  int success = false;
  emacs_value result = Qnil;

  struct Str query = copy_emacs_string(env, &bump, args[1]);
  if (!query.b) { goto err; }

  fzf_log("fzf_native_score_all START: query='%.*s'\n", (int)query.len, query.b);

  // Return all candidates if query is empty with doing anything else.
  if (query.len == 0) {
    result = args[0];
    success = true;
    goto err;
  }

  // Collect all candidates.
  // Convert list to vector to minimize calls back to Emacs.
  emacs_value collection = args[0];
  if (!env->eq(env, env->type_of(env, collection), env->intern(env, "vector"))) {
    collection = env->funcall(env, Fvconcat, 1, (emacs_value[]) { args[0] });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
      goto err;
    }
  }

  size_t batch_idx = 0, capacity;

  ptrdiff_t n = env->vec_size(env, collection);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    n = 0;
  }
  for (ptrdiff_t i = 0; i < n; i++) {
    emacs_value value = env->vec_get(env, collection, i);
    struct Str s = copy_emacs_string(env, &bump, value);
    /* If s.b is NULL here, the candidate could not be decoded even
       after `encode-coding-string' coercion. Drop it now so it doesn't
       occupy a batch slot. In practice this is rarely reached on
       Emacs 30+: the coercion path accepts almost any input. */
    if (!s.b) continue;

    if (!batches || (batches[batch_idx].len >= BATCH_SIZE && ++batch_idx >= capacity)) {
      capacity = batches ? 2 * capacity : 1;
      struct Batch *new_batches;
      if (!(new_batches = realloc(batches, capacity * sizeof *batches))) goto err;
      batches = new_batches;
      for (size_t k = batch_idx; k < capacity; ++k) batches[k].len = 0;
    }

    struct Batch *batch = batches + batch_idx;
    struct Candidate *x = batch->xs + batch->len++;
    x->value = value;
    x->s = s;
  }

  if (!batches) {
    return Qnil;
  }

  fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, false, query.b, true);
  struct Shared shared = {
    .pattern = pattern,
    .batches = batches,
    .remaining = batch_idx + 1,
  };

#ifdef _WIN32
  worker_routine(&shared);
#endif
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  // Print the shared value.
  /* ssize_t value = atomic_load(&shared.remaining); */
  /* printf("shared Remaining: %zd\n", value); */
  // Set up max number of workers according to processor.
  // It's 8 on M1 Macbook.
  unsigned max_workers = sysconf(_SC_NPROCESSORS_ONLN);

  if (!(data = malloc(sizeof *data + max_workers * sizeof *data->threads))) {
    fzf_free_pattern(pattern);
    goto err;
  }
  *data = (struct Data) { max_workers };

  unsigned num_workers = 0;
  for (; num_workers < MIN(data->max_workers, batch_idx + 1); ++num_workers)
    if (pthread_create(data->threads + num_workers, NULL, worker_routine, &shared))
      // Join all workers in order to at least safely free memory
      goto err_join_threads;
#endif
  success = true;

err_join_threads:
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  // Wait for all worker threads
  for (unsigned i = 0; i < num_workers; ++i) pthread_join(data->threads[i], NULL);
#endif
  if (pattern) fzf_free_pattern(pattern);
  if (!success) goto err;
  if (env->process_input(env) == emacs_process_input_quit) goto err;

  // Compact all batches into one flat array
  size_t len = 0;
  for (size_t i = 0; i <= batch_idx; ++i) {
    len += batches[i].len;
  }

  struct Candidate *xs = malloc(len * sizeof *xs);
  if (!xs) goto err;

  size_t pos = 0;
  for (size_t i = 0; i <= batch_idx; ++i) {
    size_t n = batches[i].len;
    memcpy(xs + pos, batches[i].xs, n * sizeof *xs);
    pos += n;
  }

  counting_sort_candidates(xs, len);

  for (size_t i = len; i-- > 0;) {
    /* printf("zero: %jd one: %jd score: %d", */
    /*        env->extract_integer(env, Qzero), */
    /*        env->extract_integer(env, Qone), */
    /*        xs[i].score); */
    /* e.g. (put-text-property 0 1 'completion-score score x) */
    if (xs[i].s.len > 0) {
      env->funcall(env, Fput_text_property, 5,
                   (emacs_value[]) {
                     Qzero, Qone, Qcompletion_score,
                     env->make_integer(env, xs[i].score),
                     xs[i].value,
                   });
    }

    result = env->funcall(env, Fcons, 2, (emacs_value[]) { xs[i].value, result });
  }

  fzf_log("fzf_native_score_all DONE: query='%.*s' count=%zu\n", (int)query.len, query.b, n);
  free(xs);

err:
  free(batches);
  bump_free(bump);
  free(data);

  if (!success
      && env->non_local_exit_check(env) == emacs_funcall_exit_return) {
    /* Only signal a generic error if no more specific signal (such as
       a `wrong-type-argument' from candidate validation) is already
       pending. Otherwise we'd clobber the better diagnostic. */
    env->non_local_exit_signal(env, env->intern(env, "error"), Qnil);
  }
  return result;
}

/* Signal `(wrong-type-argument stringp VALUE)' if VALUE is not a string.
   Returns true on failure (caller should return immediately). */
static bool signal_if_not_string(emacs_env *env, emacs_value value) {
  if (env->eq(env, env->type_of(env, value), env->intern(env, "string"))) {
    return false;
  }
  emacs_value data_args[] = { env->intern(env, "stringp"), value };
  env->non_local_exit_signal(env, env->intern(env, "wrong-type-argument"),
                              env->funcall(env, Flist, 2, data_args));
  return true;
}

// fzf-native-score STR QUERY &optional SLAB
emacs_value fzf_native_score(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void UNUSED(*data_ptr)) {
  if (signal_if_not_string(env, args[0]) || signal_if_not_string(env, args[1])) {
    return Qnil;
  }

  // Short-circuit if QUERY is empty.
  ptrdiff_t query_len;
  if (!env->copy_string_contents(env, args[1], NULL, &query_len)) {
    /* Length probe failed (likely unicode-string-p on invalid unibyte).
       Clear the exit and let the full copy path try coercion below. */
    env->non_local_exit_clear(env);
    query_len = 0;
  } else if (query_len == /* solely null byte */ 1) {
    return Qlistofzero;
  }

  // Short-circuit if STR is empty.
  ptrdiff_t str_len;
  if (!env->copy_string_contents(env, args[0], NULL, &str_len)) {
    env->non_local_exit_clear(env);
    str_len = 0;
  } else if (str_len == /* solely null byte */ 1) {
    return Qlistofzero;
  }

  struct Bump *bump = NULL;
  /* Default result on coercion failure: `(0)' - same shape as the
     empty-string short-circuit, meaning "no match". A string that
     cannot be coerced through `encode-coding-string' is treated as
     equivalent to a string with no matchable content. (In practice
     this path is rarely reached on Emacs 30+: encode-coding-string
     accepts almost any input and round-trips it to a byte sequence
     that fzf can score normally. Keeping the fallback as a safety
     net for the truly pathological case.) Also fixes a latent UB
     where `result' was used uninitialized on goto err. */
  emacs_value result = Qlistofzero;

  struct Str str = copy_emacs_string(env, &bump, args[0]);
  if (!str.b) { goto err; }

  struct Str query = copy_emacs_string(env, &bump, args[1]);
  if (!query.b) { goto err; }

  fzf_log("fzf_native_score: str='%.*s' query='%.*s'\n", (int)str.len, str.b, (int)query.len, query.b);

  /* fzf_case_mode enum : CaseSmart = 0, CaseIgnore, CaseRespect
   * normalize bool     : Always set to false because its not implemented yet.
   *                      This is reserved for future use
   * pattern char*      : Pattern you want to match. e.g. "src | lua !.c$
   * fuzzy bool         : Enable or disable fuzzy matching
   */
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseSmart, false, query.b, true);
  if (!pattern) { goto err; }

  fzf_slab_t *slab;
  if (nargs > 2) {
    // Re-use SLAB argument.
    slab = env->get_user_ptr(env, args[2]);
  } else {
    // Create a one-time use slab.
    slab = fzf_make_default_slab();
  }

  /* You can get the score/position for as many items as you want */
  int score = fzf_get_score(str.b, pattern, slab);
  fzf_position_t *pos = fzf_get_positions(str.b, pattern, slab);

  size_t offset = 1;
  size_t len = 0;
  if (pos) {
    len = pos->size;
  }

  emacs_value *result_array = malloc(sizeof(emacs_value) * (offset + len));

  result_array[0] = env->make_integer(env, score);

  for (size_t i = 0; i < len; i++) {
    result_array[offset + i] = env->make_integer(env, pos->data[len - (i + 1)]);
  }

  result = env->funcall(env, Flist, offset + len, result_array);
  fzf_free_positions(pos);
  fzf_free_pattern(pattern);

  if (nargs > 2) {
    // SLAB argument should not immediately be freed.
  } else {
    // Free one-time use slab.
    fzf_free_slab(slab);
  }

err:
  bump_free(bump);
  /* On coercion failure we return Qlistofzero (no match) rather than
     signaling, so a single un-coerceable input doesn't blow up a
     larger completion batch. Empty STR/QUERY short-circuit to the
     same value above. */
  return result;
}

void slab_finalize(void *object) {
  fzf_slab_t *slab = (fzf_slab_t *)object;
  fzf_free_slab(slab);
}

emacs_value fzf_native_make_default_slab(emacs_env *env,
                                         ptrdiff_t UNUSED(nargs),
                                         emacs_value UNUSED(args[]),
                                         void UNUSED(*data_ptr)) {
  fzf_slab_t *slab = fzf_make_default_slab();

  return env->make_user_ptr(env, slab_finalize, slab);
}

emacs_value fzf_native_make_slab(emacs_env *env,
                                 ptrdiff_t UNUSED(nargs),
                                 emacs_value args[],
                                 void UNUSED(*data_ptr)) {
  size_t slab16Size = env->extract_integer(env, args[0]);
  size_t slab32Size = env->extract_integer(env, args[1]);

  fzf_slab_t *slab = fzf_make_slab((fzf_slab_config_t){slab16Size, slab32Size});

  return env->make_user_ptr(env, slab_finalize, slab);
}

/* ================================================================
   Async shell completion
   ================================================================ */

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)

#define ASYNC_LINE_MAX   8192
#define ARENA_CHUNK_SIZE (4 * 1024 * 1024)  /* 4 MB per chunk */

/* Chunked candidate-pointer storage.
 *
 * The candidate-pointer array is split into fixed-size blocks owned by a
 * top-level pointer table.  Reader appends to the current block; when a
 * block fills, it allocates the next one.  No realloc ever moves pointer
 * data, so the worst-case allocation the reader performs is a single
 * block — predictable cost regardless of pool size.
 *
 *   cands_top[]        :  CANDS_TOP_CAP slots × 8 B  (~32 KB, fixed)
 *   cands_top[i]       :  CANDS_BLOCK_SIZE × 8 B     (2 MB, on demand)
 *
 * Defaults: 256 K pointers per block, 4096 blocks → 1 G candidates max.
 */
#define CANDS_BLOCK_SHIFT 18
#define CANDS_BLOCK_SIZE  ((size_t)1 << CANDS_BLOCK_SHIFT)
#define CANDS_BLOCK_MASK  (CANDS_BLOCK_SIZE - 1)
#define CANDS_TOP_CAP     4096

/* Arena allocator: strings are packed into large chunks so freeing the
   entire candidate set is O(chunks) instead of O(candidates). */
typedef struct ArenaChunk { struct ArenaChunk *next; size_t used; char data[]; } ArenaChunk;
typedef struct { ArenaChunk *head; } Arena;

static char *arena_strdup(Arena *a, const char *s, size_t len) {
  size_t need = len + 1;
  if (!a->head || a->head->used + need > ARENA_CHUNK_SIZE) {
    size_t chunk_sz = sizeof(ArenaChunk) + (need > ARENA_CHUNK_SIZE ? need : ARENA_CHUNK_SIZE);
    ArenaChunk *c = malloc(chunk_sz);
    if (!c) return NULL;
    c->used = 0; c->next = a->head; a->head = c;
  }
  char *p = a->head->data + a->head->used;
  memcpy(p, s, len + 1);
  a->head->used += need;
  return p;
}

static void arena_free(Arena *a) {
  ArenaChunk *c = a->head;
  while (c) { ArenaChunk *nx = c->next; free(c); c = nx; }
  a->head = NULL;
}

/* Strip ANSI CSI escape sequences (ESC [ ... m) in-place. */
static size_t async_strip_ansi(char *s, size_t len) {
  size_t r = 0, w = 0;
  while (r < len) {
    if (s[r] == 0x1b && r + 1 < len && s[r + 1] == '[') {
      r += 2;
      while (r < len && s[r] != 'm') r++;
      if (r < len) r++;
    } else {
      s[w++] = s[r++];
    }
  }
  s[w] = '\0';
  return w;
}

/* idx is the candidate's position in the scoring-time snapshot.  For
   full-pool scoring this is the index in s->cands.  For refinement
   scoring (Phase 2) it is the index in s->cands of the candidate that
   was looked up via the prefix entry's matched_idx, or in the delta
   range.  Same struct size as before on 64-bit (8 + 4 + 4 = 16). */
typedef struct { char *str; int score; uint32_t idx; } ScoredStr;

/* =====================================================================
 * Result cache (phase 1: exact-match, stale-while-revalidate)
 * =====================================================================
 * Per-session LRU keyed by query string.  Each entry holds a copy of
 * the top-K ScoredStr published when the query was last scored, plus
 * the candidate-pool size (s->count) at that moment.  When dispatch
 * sees an exact hit at the current pool size, it can return the
 * cached result without scheduling new scoring.  An older pool size
 * means new candidates have arrived since: still return the cached
 * result, but schedule a re-score so the next dispatch picks up the
 * fresh data.
 *
 * ScoredStr.str points into AsyncSession.arena, which outlives every
 * cache entry, so the cache stores those pointers directly without
 * copying string bytes.  Only the top[] array and the query string
 * itself are owned by the cache.
 *
 * Reads happen at dispatch (Emacs main thread); writes happen at
 * scoring-thread publish.  Both serialize through `cache->mu`.
 */

#define ASYNC_CACHE_MAX_ENTRIES 20
/* Reference-counted immutable index array.  Allocated once, retained by each
   consumer in O(1) (atomic refcount bump under the cache mutex — no memcpy),
   and freed when the last consumer releases it.  Eliminates the multi-hundred-
   millisecond dup_idx() stall that previously blocked the Emacs main thread
   for large match sets (while-no-input cannot interrupt a blocking C call). */
typedef struct {
  _Atomic uint32_t refcount;
  size_t           count;
  uint32_t         idx[];   /* flexible array */
} SharedIdx;

static SharedIdx *shared_idx_alloc(const uint32_t *src, size_t n) {
  if (!n || !src) return NULL;
  SharedIdx *p = malloc(sizeof *p + n * sizeof *p->idx);
  if (!p) return NULL;
  atomic_init(&p->refcount, 1);
  p->count = n;
  memcpy(p->idx, src, n * sizeof *p->idx);
  return p;
}
static SharedIdx *shared_idx_retain(SharedIdx *p) {
  if (p) atomic_fetch_add_explicit(&p->refcount, 1, memory_order_relaxed);
  return p;
}
static void shared_idx_release(SharedIdx *p) {
  if (p && atomic_fetch_sub_explicit(&p->refcount, 1, memory_order_acq_rel) == 1)
    free(p);
}

typedef struct CacheEntry {
  struct CacheEntry *prev;        /* toward head (MRU); NULL at head */
  struct CacheEntry *next;        /* toward tail (LRU); NULL at tail */
  char              *query;       /* strdup'd, owned */
  size_t             pool_gen;    /* s->count at score time */
  ScoredStr         *top;         /* malloc'd copy of published top-K */
  size_t             top_count;
  /* Phase 2: full match set (indices into s->cands at score time).
     Reference-counted SharedIdx — retained without copying at lookup. */
  SharedIdx         *matched_idx;
} CacheEntry;

typedef struct {
  pthread_mutex_t mu;
  CacheEntry     *head;        /* most recently used */
  CacheEntry     *tail;        /* least recently used */
  size_t          count;
} Cache;

static void cache_init(Cache *c) {
  pthread_mutex_init(&c->mu, NULL);
  c->head = c->tail = NULL;
  c->count = 0;
}

static void cache_free_entry(CacheEntry *e) {
  if (!e) return;
  free(e->query);
  free(e->top);
  shared_idx_release(e->matched_idx);
  free(e);
}

static void cache_destroy(Cache *c) {
  CacheEntry *e = c->head;
  while (e) { CacheEntry *n = e->next; cache_free_entry(e); e = n; }
  c->head = c->tail = NULL;
  c->count = 0;
  pthread_mutex_destroy(&c->mu);
}

/* Detach `e` from the list. Caller holds c->mu. */
static void cache_unlink(Cache *c, CacheEntry *e) {
  if (e->prev) e->prev->next = e->next; else c->head = e->next;
  if (e->next) e->next->prev = e->prev; else c->tail = e->prev;
  e->prev = e->next = NULL;
}

/* Push `e` to the head (MRU). Caller holds c->mu. */
static void cache_push_head(Cache *c, CacheEntry *e) {
  e->prev = NULL;
  e->next = c->head;
  if (c->head) c->head->prev = e;
  c->head = e;
  if (!c->tail) c->tail = e;
}

/* Helper: malloc a copy of a ScoredStr array.  Returns NULL when n=0
   (caller treats as "no top-K").  On alloc failure returns NULL and
   the caller decides whether that is fatal (it usually isn't — the
   cache is best-effort). */
static ScoredStr *dup_scored(const ScoredStr *src, size_t n) {
  if (!n) return NULL;
  ScoredStr *out = malloc(n * sizeof *out);
  if (out && src) memcpy(out, src, n * sizeof *out);
  return out;
}

/* Find an entry by query and bump it to head. Caller holds c->mu. */
static CacheEntry *cache_lookup_locked(Cache *c, const char *query) {
  for (CacheEntry *e = c->head; e; e = e->next) {
    if (strcmp(e->query, query) == 0) {
      if (e != c->head) {
        cache_unlink(c, e);
        cache_push_head(c, e);
      }
      return e;
    }
  }
  return NULL;
}

/* Exact-match lookup.  On hit, bumps LRU, copies the cached top[] out and
   retains (O(1) refcount bump) the SharedIdx so the caller can release the
   cache mutex before working with the data.  Returns true on hit, false on
   miss or top alloc failure (treated the same as a miss). */
static bool cache_lookup_exact(Cache *c, const char *query,
                               ScoredStr **out_top, size_t *out_top_count,
                               SharedIdx **out_sidx,
                               size_t *out_pool_gen) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *e = cache_lookup_locked(c, query);
  if (!e) { pthread_mutex_unlock(&c->mu); return false; }

  ScoredStr *top_copy = dup_scored(e->top, e->top_count);
  if (e->top_count && !top_copy) { pthread_mutex_unlock(&c->mu); return false; }

  /* O(1): just bump the refcount while holding the mutex. */
  SharedIdx *sidx = shared_idx_retain(e->matched_idx);

  *out_top       = top_copy;
  *out_top_count = e->top_count;
  *out_sidx      = sidx;
  *out_pool_gen  = e->pool_gen;
  pthread_mutex_unlock(&c->mu);
  return true;
}

/* Subsumption rule (Phase 2 v1, conservative):
     Q' subsumes Q (matches(Q) ⊆ matches(Q')) iff
       - neither Q nor Q' contains '|' (no OR clauses), and
       - Q starts with Q' as a textual prefix.
   This catches: extending a term ("fo" → "foo"), adding new AND
   terms ("fo" → "fo bar"), adding negations ("fo" → "fo !x"), and
   anchors.  It rejects every OR query — those fall through to
   full-pool scoring. */
static bool subsumes(const char *q_prime, const char *q) {
  if (strchr(q_prime, '|') || strchr(q, '|')) return false;
  size_t lp = strlen(q_prime);
  if (lp == 0) return true;
  size_t lq = strlen(q);
  if (lq < lp) return false;
  return memcmp(q, q_prime, lp) == 0;
}

/* Walk the cache for the longest Q' (other than Q itself) that subsumes Q.
   On hit, bumps the chosen entry to MRU, copies its top[] out and retains
   (O(1)) the SharedIdx.  Returns true on hit, false otherwise (miss or
   top alloc failure). */
static bool cache_lookup_prefix(Cache *c, const char *query,
                                ScoredStr **out_top, size_t *out_top_count,
                                SharedIdx **out_sidx,
                                size_t *out_pool_gen) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *best = NULL;
  size_t      best_len = 0;
  for (CacheEntry *e = c->head; e; e = e->next) {
    if (strcmp(e->query, query) == 0) continue;       /* skip exact */
    if (!subsumes(e->query, query)) continue;
    size_t len = strlen(e->query);
    if (len > best_len) { best = e; best_len = len; }
  }
  if (!best) { pthread_mutex_unlock(&c->mu); return false; }

  ScoredStr *top_copy = dup_scored(best->top, best->top_count);
  if (best->top_count && !top_copy) { pthread_mutex_unlock(&c->mu); return false; }

  /* O(1): just bump the refcount while holding the mutex. */
  SharedIdx *sidx = shared_idx_retain(best->matched_idx);

  if (best != c->head) { cache_unlink(c, best); cache_push_head(c, best); }

  *out_top       = top_copy;
  *out_top_count = best->top_count;
  *out_sidx      = sidx;
  *out_pool_gen  = best->pool_gen;
  pthread_mutex_unlock(&c->mu);
  return true;
}

/* Insert (or update) an entry for `query`.  All allocations are done OUTSIDE
   the mutex so the critical section is O(1) pointer swaps only — no memcpy
   of potentially-huge index arrays while the Emacs main thread waits.
   matched_idx is stored only for non-OR queries (OR queries can never be
   prefix-refinement sources).  Cache is best-effort: no-op on alloc failure.
   Evicted entries are freed after the mutex is released. */
static void cache_insert(Cache *c, const char *query, size_t pool_gen,
                         const ScoredStr *top, size_t top_count,
                         const uint32_t *matched_idx, size_t match_count) {
  bool store_idx = (matched_idx && match_count && !strchr(query, '|'));

  /* Pre-allocate ALL resources OUTSIDE the mutex. */
  ScoredStr  *new_top  = dup_scored(top, top_count);
  SharedIdx  *new_sidx = store_idx ? shared_idx_alloc(matched_idx, match_count) : NULL;
  CacheEntry *fresh    = calloc(1, sizeof *fresh);
  char       *qdup     = fresh ? strdup(query) : NULL;
  if ((top_count && !new_top) || (fresh && !qdup)) {
    free(new_top); shared_idx_release(new_sidx); free(fresh); free(qdup); return;
  }

  pthread_mutex_lock(&c->mu);

  CacheEntry *existing = cache_lookup_locked(c, query);
  if (existing) {
    ScoredStr *old_top  = existing->top;
    SharedIdx *old_sidx = existing->matched_idx;
    existing->top         = new_top;
    existing->top_count   = new_top ? top_count : 0;
    existing->matched_idx = new_sidx;
    existing->pool_gen    = pool_gen;
    pthread_mutex_unlock(&c->mu);
    free(old_top); shared_idx_release(old_sidx);
    free(qdup); free(fresh);
    return;
  }

  if (!fresh) {
    pthread_mutex_unlock(&c->mu);
    free(new_top); shared_idx_release(new_sidx);
    return;
  }
  fresh->query       = qdup;
  fresh->top         = new_top;
  fresh->top_count   = new_top ? top_count : 0;
  fresh->matched_idx = new_sidx;
  fresh->pool_gen    = pool_gen;

  CacheEntry *evicted = NULL;
  if (c->count >= ASYNC_CACHE_MAX_ENTRIES) {
    evicted = c->tail;
    if (evicted) { cache_unlink(c, evicted); c->count--; }
  }
  cache_push_head(c, fresh);
  c->count++;
  pthread_mutex_unlock(&c->mu);

  if (evicted) cache_free_entry(evicted);
}

typedef struct {
  pthread_t     reader;
  pid_t         pid;
  FILE         *fp;
  _Atomic bool stop;

  pthread_mutex_t mu;
  Arena           arena;     /* backing storage for all candidate strings */
  char          **cands_top[CANDS_TOP_CAP]; /* chunked pointer array; entries are 2 MB blocks allocated on demand */
  _Atomic size_t  count;     /* written by reader (under mu); read by any thread */
  _Atomic int     gen;

  size_t          last_filtered;   /* candidates matching last filter */
  size_t          last_total;      /* total candidates at last call */

  /* Background scoring thread */
  pthread_t        score_thread;
  pthread_mutex_t  score_req_mu;
  pthread_cond_t   score_req_cond;
  char            *score_req_filter;  /* owned; NULL = nothing pending */
  size_t           score_req_limit;
  bool             score_req_stop;
  _Atomic bool     score_abort;       /* set to cancel in-flight workers */

  char            *score_current_filter; /* filter being actively scored (under score_req_mu) */
  size_t           score_current_limit;

  /* Phase 2 refinement: when set, scoring scores against
     score_req_refine_idx ∪ s->cands[score_req_refine_delta_from..)
     instead of the full pool.  refine_idx is a SharedIdx retained at
     dispatch and released when the scoring thread steals it. */
  SharedIdx       *score_req_refine_idx;        /* may be NULL; ref-counted */
  size_t           score_req_refine_delta_from;
  bool             score_req_has_refine;

  pthread_mutex_t  score_res_mu;
  ScoredStr       *score_results;     /* latest scored+sorted results */
  size_t           score_count;       /* number of entries in score_results */

  Cache            cache;             /* per-session result cache */
} AsyncSession;

static void *async_reader(void *arg) {
  AsyncSession *s = arg;
  fzf_log("async_reader START: pid=%d\n", (int)s->pid);
  char line[ASYNC_LINE_MAX];
  while (!atomic_load_explicit(&s->stop, memory_order_relaxed) && s->fp && fgets(line, sizeof line, s->fp)) {
    size_t len = strlen(line);
    while (len && (line[len - 1] == '\n' || line[len - 1] == '\r'))
      line[--len] = '\0';
    len = async_strip_ansi(line, len);
    if (!len) continue;

    char *dup = arena_strdup(&s->arena, line, len);
    if (!dup) continue;

    /* Chunked append: figure out which block this index lands in, allocate
       the block on first use, write the slot, publish via the count
       store-release.  The largest single allocation we ever do here is
       one block (CANDS_BLOCK_SIZE * 8 B = 2 MB) — predictable cost
       regardless of pool size, so memory-pressure stalls scale O(1). */
    size_t cur = atomic_load_explicit(&s->count, memory_order_relaxed);
    size_t hi  = cur >> CANDS_BLOCK_SHIFT;
    size_t lo  = cur & CANDS_BLOCK_MASK;
    if (hi >= CANDS_TOP_CAP) continue;             /* 1 G-candidate ceiling */

    if (s->cands_top[hi] == NULL) {
      char **blk = malloc(CANDS_BLOCK_SIZE * sizeof *blk);
      if (!blk) continue;
      fzf_log("async_reader: allocated cands block %zu (%zu entries, %zu MB)\n",
              hi, (size_t)CANDS_BLOCK_SIZE,
              (size_t)(CANDS_BLOCK_SIZE * sizeof *blk) / (1024 * 1024));
      pthread_mutex_lock(&s->mu);
      s->cands_top[hi] = blk;
      pthread_mutex_unlock(&s->mu);
    }
    pthread_mutex_lock(&s->mu);
    s->cands_top[hi][lo] = dup;
    atomic_store_explicit(&s->count, cur + 1, memory_order_release);
    pthread_mutex_unlock(&s->mu);
    atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
  }
  fzf_log("async_reader EXIT: total=%zu gen=%d\n",
          (size_t)atomic_load_explicit(&s->count, memory_order_relaxed),
          (int)atomic_load_explicit(&s->gen, memory_order_relaxed));
  return NULL;
}

static void *scoring_thread_fn(void *arg);  /* defined after async_scoring_worker */

static void async_session_destroy(void *ptr) {
  AsyncSession *s = ptr;
  if (!s) return;
  fzf_log("async_session_destroy: pid=%d count=%zu\n", (int)s->pid,
          (size_t)atomic_load_explicit(&s->count, memory_order_relaxed));

  /* Signal everything to stop simultaneously so scoring and reader wind down
     in parallel rather than sequentially. */
  atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);
  atomic_store_explicit(&s->stop, true, memory_order_relaxed);
  if (s->pid > 0) kill(s->pid, SIGTERM);   /* reader unblocks on pipe EOF */

  pthread_mutex_lock(&s->score_req_mu);
  free(s->score_req_filter);
  s->score_req_filter = NULL;
  shared_idx_release(s->score_req_refine_idx);
  s->score_req_refine_idx = NULL;
  s->score_req_has_refine = false;
  s->score_req_stop   = true;
  pthread_cond_signal(&s->score_req_cond);
  pthread_mutex_unlock(&s->score_req_mu);
  pthread_join(s->score_thread, NULL);

  free(s->score_results);
  free(s->score_current_filter);
  pthread_mutex_destroy(&s->score_res_mu);
  pthread_mutex_destroy(&s->score_req_mu);
  pthread_cond_destroy(&s->score_req_cond);

  /* Reader has been winding down since SIGTERM above; join it now. */
  pthread_join(s->reader, NULL);
  if (s->fp)      { fclose(s->fp); s->fp = NULL; }
  if (s->pid > 0) { waitpid(s->pid, NULL, 0); s->pid = -1; }

  cache_destroy(&s->cache);

  pthread_mutex_lock(&s->mu);
  arena_free(&s->arena);
  for (size_t i = 0; i < CANDS_TOP_CAP; i++) {
    if (s->cands_top[i]) { free(s->cands_top[i]); s->cands_top[i] = NULL; }
  }
  pthread_mutex_unlock(&s->mu);
  pthread_mutex_destroy(&s->mu);
  free(s);
}

/* fzf-native-async-start COMMAND &optional DIR -> session handle */
static emacs_value
fzf_native_async_start(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *UNUSED(data)) {
  ptrdiff_t len = 0;
  env->copy_string_contents(env, args[0], NULL, &len);
  char *cmd = malloc((size_t)len);
  if (!cmd) return Qnil;
  env->copy_string_contents(env, args[0], cmd, &len);

  char *dir = NULL;
  if (nargs > 1 && !env->eq(env, args[1], Qnil)) {
    ptrdiff_t dlen = 0;
    env->copy_string_contents(env, args[1], NULL, &dlen);
    dir = malloc((size_t)dlen);
    if (dir) env->copy_string_contents(env, args[1], dir, &dlen);
  }

  int pfd[2];
  if (pipe(pfd) != 0) {
    fzf_log("async_start: pipe failed\n");
    free(cmd);
    free(dir);
    return Qnil;
  }

  pid_t pid = fork();
  if (pid < 0) {
    fzf_log("async_start: fork failed\n");
    close(pfd[0]);
    close(pfd[1]);
    free(cmd);
    free(dir);
    return Qnil;
  }

  if (pid == 0) {
    close(pfd[0]);
    dup2(pfd[1], STDOUT_FILENO);
    close(pfd[1]);
    int dn = open("/dev/null", O_WRONLY);
    if (dn >= 0) { dup2(dn, STDERR_FILENO); close(dn); }
    if (dir) chdir(dir);
    execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
    _exit(127);
  }
  close(pfd[1]);

  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) {
    fzf_log("async_start: calloc failed\n");
    close(pfd[0]);
    kill(pid, SIGTERM);
    waitpid(pid, NULL, 0);
    free(cmd);
    free(dir);
    return Qnil;
  }

  fzf_log("async_start: cmd='%s' dir='%s' pid=%d\n",
          cmd, dir ? dir : ".", (int)pid);

  free(cmd);
  free(dir);

  s->pid   = pid;
  s->fp    = fdopen(pfd[0], "r");
  /* cands_top[] was zeroed by calloc(); blocks are allocated lazily by
     the reader.  No upfront pointer-array allocation. */
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  cache_init(&s->cache);
  atomic_store(&s->gen, 0);
  atomic_store(&s->score_abort, false);

  if (!s->fp ||
      pthread_create(&s->reader, NULL, async_reader, s) != 0 ||
      pthread_create(&s->score_thread, NULL, scoring_thread_fn, s) != 0) {
    async_session_destroy(s);
    return Qnil;
  }
  return env->make_user_ptr(env, async_session_destroy, s);
}

/* fzf-native-async-stop HANDLE */
static emacs_value
fzf_native_async_stop(emacs_env *env, ptrdiff_t nargs,
                      emacs_value args[], void *UNUSED(data)) {
  (void)nargs;
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (s) {
    fzf_log("async_stop: pid=%d total=%zu\n", (int)s->pid,
            (size_t)atomic_load_explicit(&s->count, memory_order_relaxed));
    env->set_user_ptr(env, args[0], NULL);
    async_session_destroy(s);
  }
  return Qnil;
}

/* fzf-native-async-generation HANDLE -> integer */
static emacs_value
fzf_native_async_generation(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *UNUSED(data)) {
  (void)nargs;
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;
  return env->make_integer(env,
    atomic_load_explicit(&s->gen, memory_order_relaxed));
}

static int cmp_scored_desc(const void *a, const void *b) {
  return ((const ScoredStr *)b)->score - ((const ScoredStr *)a)->score;
}

/* Counting sort of xs[0..n-1] by score, descending.
   O(n + max_score). Falls back to qsort if allocations fail. */
static void counting_sort_scored(ScoredStr *xs, size_t n) {
  if (n <= 1) return;
  int max_score = 0;
  for (size_t i = 0; i < n; i++)
    if (xs[i].score > max_score) max_score = xs[i].score;

  int *count = calloc((size_t)(max_score + 1), sizeof *count);
  if (!count) { qsort(xs, n, sizeof *xs, cmp_scored_desc); return; }

  for (size_t i = 0; i < n; i++) count[xs[i].score]++;

  /* Convert counts to start positions for descending order. */
  int pos = 0;
  for (int s = max_score; s >= 0; s--) { int c = count[s]; count[s] = pos; pos += c; }

  ScoredStr *out = malloc(n * sizeof *out);
  if (!out) { free(count); qsort(xs, n, sizeof *xs, cmp_scored_desc); return; }

  for (size_t i = 0; i < n; i++) out[count[xs[i].score]++] = xs[i];
  memcpy(xs, out, n * sizeof *xs);
  free(out);
  free(count);
}

/* Lightweight work unit: a contiguous range of virtual indices to score.
   No string data is embedded — workers resolve candidates from cands_top. */
struct AsyncScoringRange {
  size_t from;
  size_t to;
};

/* Shared read-only context for all scoring workers. */
struct AsyncScoringShared {
  AsyncSession  *s;
  fzf_pattern_t *pattern;
  _Atomic bool  *stop;

  /* Refine context: scattered previously-matched entries (bounded by LIMIT).
     NULL for full-pool runs. */
  char     **refine_snap;
  uint32_t  *refine_idx_snap;
  size_t     refine_count;
  size_t     refine_delta_from;
  bool       has_refine;

  /* Work distribution */
  struct AsyncScoringRange *ranges;
  _Atomic ssize_t           remaining;

  /* Per-worker result cap (0 = unlimited).  Set to a multiple of
     ceil(limit/num_workers) to prevent memory exhaustion on broad queries. */
  size_t per_worker_limit;
};

/* Per-worker state: each spawned thread gets its own copy. */
struct AsyncWorkerArgs {
  struct AsyncScoringShared *shared;
  ScoredStr *results;
  size_t     result_count;
  size_t     result_cap;
};

static void *async_scoring_worker(void *ptr) {
  struct AsyncWorkerArgs    *args   = ptr;
  struct AsyncScoringShared *shared = args->shared;
  AsyncSession  *s       = shared->s;
  fzf_pattern_t *pattern = shared->pattern;
  fzf_slab_t    *slab    = fzf_make_default_slab();

  ssize_t ri;
  while ((ri = atomic_fetch_sub_explicit(&shared->remaining, 1,
                                         memory_order_seq_cst) - 1) >= 0) {
    if (shared->stop && atomic_load_explicit(shared->stop, memory_order_relaxed))
      break;
    if (shared->per_worker_limit && args->result_count >= shared->per_worker_limit)
      break;

    struct AsyncScoringRange *range = &shared->ranges[ri];
    bool aborted = false;
    for (size_t i = range->from; i < range->to; i++) {
      if ((i & 0xFF) == 0 && shared->stop &&
          atomic_load_explicit(shared->stop, memory_order_relaxed)) {
        aborted = true; break;
      }

      /* Resolve candidate: scattered refine entries first, then cands_top. */
      const char *str;
      uint32_t    orig_idx;
      if (shared->has_refine && i < shared->refine_count) {
        str      = shared->refine_snap[i];
        orig_idx = shared->refine_idx_snap[i];
      } else {
        size_t gi = shared->has_refine
                    ? shared->refine_delta_from + (i - shared->refine_count)
                    : i;
        str      = s->cands_top[gi >> CANDS_BLOCK_SHIFT][gi & CANDS_BLOCK_MASK];
        orig_idx = (uint32_t)gi;
      }

      int sc = pattern ? fzf_get_score(str, pattern, slab) : 1;
      if (pattern && sc <= 0) continue;

      if (args->result_count >= args->result_cap) {
        size_t newcap = args->result_cap ? args->result_cap * 2 : 64;
        ScoredStr *nb = realloc(args->results, newcap * sizeof *nb);
        if (!nb) { aborted = true; break; }
        args->results    = nb;
        args->result_cap = newcap;
      }
      args->results[args->result_count++] =
          (ScoredStr){ .str = (char *)str, .score = sc, .idx = orig_idx };

      if (shared->per_worker_limit && args->result_count >= shared->per_worker_limit)
        break;
    }
    if (aborted) break;
  }

  fzf_free_slab(slab);
  return NULL;
}

static void *scoring_thread_fn(void *arg) {
  AsyncSession *s = arg;
  fzf_log("scoring_thread START\n");

  for (;;) {
    pthread_mutex_lock(&s->score_req_mu);
    while (!s->score_req_stop && !s->score_req_filter)
      pthread_cond_wait(&s->score_req_cond, &s->score_req_mu);
    if (s->score_req_stop) {
      pthread_mutex_unlock(&s->score_req_mu);
      break;
    }
    char  *filter = s->score_req_filter;   /* steal ownership */
    size_t limit  = s->score_req_limit;
    s->score_req_filter = NULL;
    /* Phase 2: also steal refine params (NULL if no refine). */
    bool       has_refine        = s->score_req_has_refine;
    SharedIdx *refine_sidx       = s->score_req_refine_idx;
    size_t     refine_delta_from = s->score_req_refine_delta_from;
    s->score_req_refine_idx        = NULL;
    s->score_req_refine_delta_from = 0;
    s->score_req_has_refine        = false;
    /* Record what we're about to score so main thread can skip abort for same filter */
    free(s->score_current_filter);
    s->score_current_filter = strdup(filter);
    s->score_current_limit  = limit;
    pthread_mutex_unlock(&s->score_req_mu);

    /* Reset abort flag AFTER stealing request so we don't race with the
       next dispatch that may have already set it again. */
    atomic_store_explicit(&s->score_abort, false, memory_order_seq_cst);

    /* Capture current pool count (brief lock). */
    pthread_mutex_lock(&s->mu);
    size_t pool_count = atomic_load_explicit(&s->count, memory_order_relaxed);
    pthread_mutex_unlock(&s->mu);

    /* Cap refine inputs against the current pool (defensive). */
    if (has_refine && refine_delta_from > pool_count)
      refine_delta_from = pool_count;
    size_t refine_count = has_refine ? refine_sidx->count : 0;
    size_t delta_count  = has_refine ? (pool_count - refine_delta_from) : 0;
    size_t scount       = has_refine ? (refine_count + delta_count) : pool_count;

    /* Allocate only the small arrays for scattered refine indices (bounded by
       LIMIT ≤ 10000).  Workers read full-pool and delta candidates directly
       from cands_top — no O(pool_count) snapshot or batch array is needed. */
    char     **refine_snap     = refine_count ? malloc(refine_count * sizeof *refine_snap)     : NULL;
    uint32_t  *refine_idx_snap = refine_count ? malloc(refine_count * sizeof *refine_idx_snap) : NULL;
    if (refine_count && (!refine_snap || !refine_idx_snap)) {
      pthread_mutex_lock(&s->score_req_mu);
      free(s->score_current_filter); s->score_current_filter = NULL;
      pthread_mutex_unlock(&s->score_req_mu);
      free(refine_snap); free(refine_idx_snap); shared_idx_release(refine_sidx); free(filter); continue;
    }

    pthread_mutex_lock(&s->mu);
    if (has_refine) {
      for (size_t k = 0; k < refine_count; k++) {
        uint32_t i = refine_sidx->idx[k];
        refine_snap[k]     = (i < s->count)
                              ? s->cands_top[i >> CANDS_BLOCK_SHIFT][i & CANDS_BLOCK_MASK]
                              : "";
        refine_idx_snap[k] = i;
      }
      if (s->count < pool_count) pool_count = s->count;
      if (delta_count > pool_count - refine_delta_from)
        delta_count = pool_count - refine_delta_from;
      scount = refine_count + delta_count;
    } else {
      if (s->count < pool_count) pool_count = scount = s->count;
    }
    pthread_mutex_unlock(&s->mu);

    /* Build a range array — each entry is 16 bytes, no string pointers.
       Workers resolve candidates from cands_top at score time. */
    size_t num_ranges = scount ? (scount + BATCH_SIZE - 1) / BATCH_SIZE : 0;
    struct AsyncScoringRange *ranges = num_ranges ? malloc(num_ranges * sizeof *ranges) : NULL;
    if (num_ranges && !ranges) {
      pthread_mutex_lock(&s->score_req_mu);
      free(s->score_current_filter); s->score_current_filter = NULL;
      pthread_mutex_unlock(&s->score_req_mu);
      free(refine_snap); free(refine_idx_snap); shared_idx_release(refine_sidx); free(filter); continue;
    }
    for (size_t r = 0; r < num_ranges; r++) {
      ranges[r].from = r * BATCH_SIZE;
      ranges[r].to   = (r + 1) * BATCH_SIZE <= scount ? (r + 1) * BATCH_SIZE : scount;
    }

    /* Reserve one core for Emacs's event loop and idle timers. */
    unsigned ncpu        = (unsigned)sysconf(_SC_NPROCESSORS_ONLN);
    unsigned max_workers = ncpu > 1 ? ncpu - 1 : 1;

    size_t flen = strlen(filter);
    fzf_pattern_t *pattern = flen ? fzf_parse_pattern(CaseIgnore, false, filter, true) : NULL;
    bool has_pattern = (pattern != NULL);

    /* Cap per-worker output to prevent memory exhaustion on broad/empty queries.
       4× slack so results don't cluster in a subset of workers. */
    unsigned num_workers_to_spawn = (unsigned)MIN(max_workers, num_ranges);
    size_t per_worker_limit = (limit && num_workers_to_spawn)
        ? (limit / num_workers_to_spawn + 1) * 4 : 0;

    struct AsyncScoringShared ws = {
      .s                 = s,
      .pattern           = pattern,
      .stop              = &s->score_abort,
      .refine_snap       = refine_snap,
      .refine_idx_snap   = refine_idx_snap,
      .refine_count      = refine_count,
      .refine_delta_from = refine_delta_from,
      .has_refine        = has_refine,
      .ranges            = ranges,
      .remaining         = (ssize_t)num_ranges,
      .per_worker_limit  = per_worker_limit,
    };

    struct AsyncWorkerArgs *worker_args = num_workers_to_spawn
        ? malloc(num_workers_to_spawn * sizeof *worker_args) : NULL;
    pthread_t *threads = num_workers_to_spawn
        ? malloc(num_workers_to_spawn * sizeof *threads) : NULL;
    unsigned num_workers = 0;
    if (worker_args && threads && num_ranges) {
      for (unsigned w = 0; w < num_workers_to_spawn; w++)
        worker_args[w] = (struct AsyncWorkerArgs){ .shared = &ws };
      for (; num_workers < num_workers_to_spawn; num_workers++)
        pthread_create(&threads[num_workers], NULL, async_scoring_worker,
                       &worker_args[num_workers]);
    }
    for (unsigned i = 0; i < num_workers; i++)
      pthread_join(threads[i], NULL);
    free(threads);
    if (pattern) fzf_free_pattern(pattern);

    /* If a different filter arrived while we were scoring, discard results. */
    if (atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
      pthread_mutex_lock(&s->score_req_mu);
      free(s->score_current_filter); s->score_current_filter = NULL;
      pthread_mutex_unlock(&s->score_req_mu);
      if (worker_args)
        for (unsigned w = 0; w < num_workers; w++) free(worker_args[w].results);
      free(worker_args); free(ranges);
      free(refine_snap); free(refine_idx_snap); shared_idx_release(refine_sidx); free(filter);
      continue;
    }

    /* Merge per-worker result buffers into a single flat array. */
    size_t pos = 0;
    if (worker_args)
      for (unsigned w = 0; w < num_workers; w++) pos += worker_args[w].result_count;

    ScoredStr *flat = pos ? malloc(pos * sizeof *flat) : NULL;
    size_t flat_pos = 0;
    if (flat && worker_args) {
      for (unsigned w = 0; w < num_workers; w++) {
        memcpy(flat + flat_pos, worker_args[w].results,
               worker_args[w].result_count * sizeof *flat);
        flat_pos += worker_args[w].result_count;
      }
    }
    if (worker_args)
      for (unsigned w = 0; w < num_workers; w++) free(worker_args[w].results);
    free(worker_args);
    free(ranges);

    /* Capture the full matched-index set BEFORE counting_sort
       reorders flat[].  The cache stores this for prefix-refinement
       scoring of extending queries (Phase 2). */
    uint32_t *matched_idx = NULL;
    size_t    match_count = pos;
    if (pos) {
      matched_idx = malloc(pos * sizeof *matched_idx);
      if (matched_idx) {
        for (size_t i = 0; i < pos; i++) matched_idx[i] = flat[i].idx;
      } else {
        match_count = 0;  /* alloc failure: cache entry will lack the set */
      }
    }

    if (flat && has_pattern && pos > 1)
      counting_sort_scored(flat, pos);

    size_t emit = (limit && limit < pos) ? limit : pos;

    /* Clear active-filter marker before publishing results */
    pthread_mutex_lock(&s->score_req_mu);
    free(s->score_current_filter); s->score_current_filter = NULL;
    pthread_mutex_unlock(&s->score_req_mu);

    pthread_mutex_lock(&s->score_res_mu);
    free(s->score_results);
    s->score_results = flat;
    s->score_count   = emit;
    s->last_filtered = pos;
    s->last_total    = pool_count;
    pthread_mutex_unlock(&s->score_res_mu);

    /* Cache what we just published, keyed by filter, tagged with the
       candidate-pool size at score time.  Subsequent dispatches with
       the same filter at the same pool size skip scoring entirely.
       matched_idx carries the full match set for prefix-refinement
       scoring of extending queries. */
    cache_insert(&s->cache, filter, pool_count, flat, emit,
                 matched_idx, match_count);
    free(matched_idx);

    /* Increment gen so Elisp knows new results are available */
    atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);

    fzf_log("scoring_thread: filter='%s' filtered=%zu total=%zu emit=%zu refine=%d\n",
            filter, pos, pool_count, emit, has_refine ? 1 : 0);

    free(refine_snap); free(refine_idx_snap); shared_idx_release(refine_sidx); free(filter);
  }

  fzf_log("scoring_thread EXIT\n");
  return NULL;
}

/* fzf-native-async-candidates HANDLE FILTER &optional LIMIT -> list of strings, scored.
   Returns immediately with the last completed scored results while dispatching a new
   scoring job on the background thread.  Non-blocking on the main thread. */
static emacs_value
fzf_native_async_candidates(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;

  ptrdiff_t flen = 0;
  env->copy_string_contents(env, args[1], NULL, &flen);
  char *filter = malloc((size_t)flen);
  if (!filter) return Qnil;
  env->copy_string_contents(env, args[1], filter, &flen);

  size_t limit = 0;
  if (nargs > 2 && !env->eq(env, args[2], Qnil))
    limit = (size_t)env->extract_integer(env, args[2]);

  fzf_log("async_candidates: filter='%s' limit=%zu — dispatching to bg thread\n",
          filter, limit);

  /* Read the current candidate-pool size so we can decide whether a
     cache hit is fresh (same pool size) or stale (pool grew since).
     count is _Atomic: no lock needed, so this call never blocks on
     s->mu even when the reader is in the middle of a large realloc. */
  size_t current_count = atomic_load_explicit(&s->count, memory_order_acquire);

  /* Cache lookup.  Three outcomes:
       - exact fresh:  return cached top, no scoring scheduled.
       - exact stale:  return cached top, schedule refine using this
                       entry's matched_idx + delta to current count.
       - prefix match: return prefix entry's top (a superset of Q's
                       results), schedule refine using prefix entry's
                       matched_idx + delta.
       - miss:         return current score_results (today's behavior),
                       schedule full-pool scoring. */
  ScoredStr *snap           = NULL;
  size_t     rcount         = 0;
  size_t     entry_pool_gen = 0;
  SharedIdx *entry_sidx     = NULL;

  bool exact = cache_lookup_exact(&s->cache, filter,
                                  &snap, &rcount,
                                  &entry_sidx,
                                  &entry_pool_gen);
  bool prefix = false;
  if (!exact) {
    prefix = cache_lookup_prefix(&s->cache, filter,
                                 &snap, &rcount,
                                 &entry_sidx,
                                 &entry_pool_gen);
  }
  bool exact_fresh = exact && entry_pool_gen == current_count;
  bool can_refine  = (exact || prefix) && entry_sidx != NULL;

  if (exact_fresh) {
    /* Cache fully satisfied: nothing to score. */
    free(filter);
    shared_idx_release(entry_sidx);
  } else {
    /* Schedule scoring.  If we have a prefix or stale-exact hit, pass
       the entry's matched_idx + pool_gen as refinement params so the
       scoring thread restricts its work to {entry's matches ∪ delta
       since entry was scored}. */
    pthread_mutex_lock(&s->score_req_mu);
    bool filter_changed = !(s->score_current_filter &&
                            strcmp(s->score_current_filter, filter) == 0 &&
                            s->score_current_limit == limit);
    if (filter_changed)
      atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);
    free(s->score_req_filter);
    s->score_req_filter = filter;
    s->score_req_limit  = limit;
    shared_idx_release(s->score_req_refine_idx);
    s->score_req_refine_idx        = can_refine ? entry_sidx    : NULL;
    s->score_req_refine_delta_from = can_refine ? entry_pool_gen : 0;
    s->score_req_has_refine        = can_refine;
    pthread_cond_signal(&s->score_req_cond);
    pthread_mutex_unlock(&s->score_req_mu);
    /* If we didn't pass entry_sidx into the refine slot, release it. */
    if (!can_refine) shared_idx_release(entry_sidx);
  }

  /* Cache miss (no exact, no prefix): fall back to score_results. */
  if (!exact && !prefix) {
    pthread_mutex_lock(&s->score_res_mu);
    rcount = s->score_count;
    snap   = rcount ? malloc(rcount * sizeof *snap) : NULL;
    if (snap && s->score_results)
      memcpy(snap, s->score_results, rcount * sizeof *snap);
    else
      rcount = 0;
    pthread_mutex_unlock(&s->score_res_mu);
  }

  /* Build Emacs list from stale results — strings are stable until session destroy */
  emacs_value result = Qnil;
  for (size_t i = rcount; i-- > 0;) {
    emacs_value str = env->make_string(env, snap[i].str,
                                       (ptrdiff_t)strlen(snap[i].str));
    enum emacs_funcall_exit status = env->non_local_exit_check(env);
    if (status == emacs_funcall_exit_signal) {
      env->non_local_exit_clear(env);
      continue;
    } else if (status != emacs_funcall_exit_return) {
      break;
    }
    result = env->funcall(env, Fcons, 2, (emacs_value[]){ str, result });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      break;
  }

  free(snap);
  return result;
}

/* fzf-native-async-stats HANDLE -> (filtered . total) */
static emacs_value
fzf_native_async_stats(emacs_env *env, ptrdiff_t UNUSED(nargs),
                       emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;
  pthread_mutex_lock(&s->score_res_mu);
  size_t filtered = s->last_filtered;
  size_t total    = s->last_total;
  pthread_mutex_unlock(&s->score_res_mu);
  return env->funcall(env, Fcons, 2, (emacs_value[]){
    env->make_integer(env, (intmax_t)filtered),
    env->make_integer(env, (intmax_t)total),
  });
}

#endif /* APPLE || linux || FreeBSD */

int emacs_module_init(struct emacs_runtime *rt) {
  // Verify compatability with Emacs executable loading this module
  if ((size_t) rt->size < sizeof *rt)
    return 1;
  emacs_env *env = rt->get_environment(rt);
  if ((size_t) env->size < sizeof *env)
    return 2;

  global_rt = rt;

#ifdef FZF_NATIVE_DEBUG
  /* Bootstrap the log file at ~/.emacs.d/fzf-native.log. Truncate on each
     module load so logs don't grow unboundedly across Emacs sessions. */
  if (!fzf_log_file) {
    const char *home = getenv("HOME");
    if (home) {
      char path[1024];
      int n = snprintf(path, sizeof(path), "%s/.emacs.d/fzf-native.log", home);
      if (n > 0 && (size_t)n < sizeof(path)) {
        remove(path); /* delete prior log if present; ignore error */
        fzf_log_file = fopen(path, "a");
        if (fzf_log_file) {
          fzf_log("--- fzf-native module initialized ---\n");
        }
      }
    }
  }
#endif

  static struct Data data;

  // fzf-native-score-all COLLECTION QUERY &optional SLAB
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-score-all"),
      env->make_function(env, 2, 3, fzf_native_score_all,
                         "Score COLLECTION matching QUERY.\n"
                         "\n"
                         "\\(fn COLLECTION QUERY &optional SLAB)",
                         &data),
    });

  // fzf-native-score STR QUERY &optional SLAB
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-score"),
      env->make_function(env, 2, 3, fzf_native_score,
                         "Score STR matching QUERY.\n"
                         "\n"
                         "\\(fn STR QUERY &optional SLAB)",
                         &data),
    });

  env->funcall(env, env->intern(env, "provide"), 1,
               (emacs_value[]) { env->intern(env, "fzf-native-module") });

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-start"),
      env->make_function(env, 1, 2, fzf_native_async_start,
                         "Start async shell COMMAND; return a session handle.\n"
                         "Optional DIR sets the working directory (default: Emacs cwd).\n\n"
                         "\\(fn COMMAND &optional DIR)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-stop"),
      env->make_function(env, 1, 1, fzf_native_async_stop,
                         "Stop async session HANDLE and free resources.\n\n"
                         "\\(fn HANDLE)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-generation"),
      env->make_function(env, 1, 1, fzf_native_async_generation,
                         "Return candidate-count generation for HANDLE.\n\n"
                         "\\(fn HANDLE)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-candidates"),
      env->make_function(env, 2, 3, fzf_native_async_candidates,
                         "Return fzf-scored candidates from HANDLE matching FILTER.\n"
                         "Optional LIMIT caps the number of candidates returned to Elisp;\n"
                         "use `fzf-native-async-stats' to get the full filtered count.\n\n"
                         "\\(fn HANDLE FILTER &optional LIMIT)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-stats"),
      env->make_function(env, 1, 1, fzf_native_async_stats,
                         "Return (FILTERED . TOTAL) counts from the last async-candidates call.\n\n"
                         "\\(fn HANDLE)", NULL),
    });
#endif

  // fzf-native-make-default-slab
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-make-default-slab"),
      env->make_function(env, 0, 0, fzf_native_make_default_slab,
                         "Instantiate and return a default fzf slab.\n"
                         "\n"
                         "\\(fn)",
                         &data),
    });

  env->funcall(env, env->intern(env, "provide"), 1,
               (emacs_value[]) { env->intern(env, "fzf-native-make-default-slab") });

  // fzf-native-make-slab
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-make-slab"),
      env->make_function(env, 2, 2, fzf_native_make_slab,
                         "Instantiate and return a fzf slab.\n"
                         "\n"
                         "\\(fn SIZE16 SIZE32)",
                         &data),
    });

  env->funcall(env, env->intern(env, "provide"), 1,
               (emacs_value[]) { env->intern(env, "fzf-native-make-slab") });

  // Get a few common lisp functions.
  Qt = env->make_global_ref(env, env->intern(env, "t"));
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));
  Fcons = env->make_global_ref(env, env->intern(env, "cons"));
  Flist = env->make_global_ref(env, env->intern(env, "list"));
  Fhashtablep = env->make_global_ref(env, env->intern(env, "hash-table-p"));
  Fmessage = env->make_global_ref(env, env->intern(env, "message"));
  Fvectorp = env->make_global_ref(env, env->intern(env, "vectorp"));
  Fvconcat = env->make_global_ref(env, env->intern(env, "vconcat"));
  Fconsp = env->make_global_ref(env, env->intern(env, "consp"));
  Ffunctionp = env->make_global_ref(env, env->intern(env, "functionp"));
  Fsymbolp = env->make_global_ref(env, env->intern(env, "symbolp"));
  Fsymbolname = env->make_global_ref(env, env->intern(env, "symbol-name"));
  Flength = env->make_global_ref(env, env->intern(env, "length"));
  Fnth = env->make_global_ref(env, env->intern(env, "nth"));
  Fprinc = env->make_global_ref(env, env->intern(env, "princ"));
  Freverse = env->make_global_ref(env, env->intern(env, "reverse"));
  Fcdr = env->make_global_ref(env, env->intern(env, "cdr"));
  Fcar = env->make_global_ref(env, env->intern(env, "car"));
  Qcompletion_score = env->make_global_ref(env, env->intern(env, "completion-score"));
  Fput_text_property = env->make_global_ref(env, env->intern(env, "put-text-property"));
  Fencode_coding_string = env->make_global_ref(env, env->intern(env, "encode-coding-string"));
  Qutf_8 = env->make_global_ref(env, env->intern(env, "utf-8"));
  Qlistofzero = env->make_global_ref(
    env, env->funcall(env, Fcons, 2,
                      (emacs_value[]){env->make_integer(env, 0), Qnil}));
  Qzero = env->make_global_ref(env, env->make_integer(env, 0));
  Qone = env->make_global_ref(env, env->make_integer(env, 1));

  return 0;
}
