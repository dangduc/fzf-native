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
emacs_value Qface, Qcompletions_common_part;
emacs_value Fremove_text_properties, Qface_nil_plist;


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

/* Apply `completions-common-part' face to STR_VAL on positions matched by
   PATTERN against CSTR.  Computes positions via fzf_get_positions and groups
   them into contiguous runs to minimize put-text-property calls.

   Byte offsets from fzf are used directly as character positions: accurate
   for ASCII, may be slightly misaligned for multi-byte UTF-8 (same caveat
   as the async highlight path). */
static void apply_highlight_positions(emacs_env *env,
                                      const char *cstr,
                                      fzf_pattern_t *pattern,
                                      fzf_slab_t *slab,
                                      emacs_value str_val) {
  /* Empty candidates can't carry text properties or matched positions; skip
     the funcalls and the get_positions slab work entirely. */
  if (cstr[0] == '\0') return;
  /* Strip any `completions-common-part' face left over from a prior highlight
     pass on the same Emacs string (fussy reuses caller-owned candidate strings
     across keystrokes; without this, e.g. "ab" highlight [0,2] persists when
     the user backspaces to "a" because put-text-property only writes the new
     [0,1] range and leaves byte [1,2] highlighted).  One funcall per
     highlighted candidate; the (face nil) plist is interned at init. */
  emacs_value len_v = env->funcall(env, Flength, 1, &str_val);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
    emacs_value rargs[4] = { Qzero, len_v, Qface_nil_plist, str_val };
    env->funcall(env, Fremove_text_properties, 4, rargs);
    env->non_local_exit_clear(env);
  } else {
    env->non_local_exit_clear(env);
  }
  fzf_position_t *pos = fzf_get_positions(cstr, pattern, slab);
  if (pos && pos->size > 0) {
    /* pos->data[] is in descending order: pos->data[0] = highest position.
       Iterate ascending (j from size-1 to 0) to find contiguous runs. */
    size_t plen      = pos->size;
    size_t run_start = pos->data[plen - 1];
    size_t run_end   = run_start;
    for (ptrdiff_t j = (ptrdiff_t)plen - 2; j >= 0; j--) {
      size_t p = pos->data[j];
      if (p == run_end + 1) {
        run_end = p;
      } else {
        emacs_value a[5] = {
          env->make_integer(env, (intmax_t)run_start),
          env->make_integer(env, (intmax_t)(run_end + 1)),
          Qface, Qcompletions_common_part, str_val };
        env->funcall(env, Fput_text_property, 5, a);
        env->non_local_exit_clear(env);
        run_start = run_end = p;
      }
    }
    emacs_value a[5] = {
      env->make_integer(env, (intmax_t)run_start),
      env->make_integer(env, (intmax_t)(run_end + 1)),
      Qface, Qcompletions_common_part, str_val };
    env->funcall(env, Fput_text_property, 5, a);
    env->non_local_exit_clear(env);
  }
  fzf_free_positions(pos);
}

/* Read `fzf-native-case-mode' via symbol-value and resolve to fzf_case_types.
   Recognized symbol values: smart (default), ignore, respect.
   Falls back to CaseSmart on any read or comparison failure. */
static fzf_case_types resolve_fzf_native_case_mode(emacs_env *env) {
  emacs_value sym = env->intern(env, "fzf-native-case-mode");
  emacs_value v   = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return CaseSmart;
  }
  if (env->eq(env, v, env->intern(env, "ignore")))  return CaseIgnore;
  if (env->eq(env, v, env->intern(env, "respect"))) return CaseRespect;
  return CaseSmart;
}

/* Read fussy-fzf-native-highlight via symbol-value and resolve to a cap.
   Returns:
     0    — no highlighting (nil, negative, unreadable, or zero).
     LEN  — highlight all (t).
     N    — highlight top N (clamped to LEN). */
static size_t resolve_fussy_highlight_cap(emacs_env *env, size_t len) {
  emacs_value sym = env->intern(env, "fussy-fzf-native-highlight");
  emacs_value v   = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return 0;
  }
  if (env->eq(env, v, Qnil)) return 0;
  if (env->eq(env, v, Qt))   return len;
  intmax_t n = env->extract_integer(env, v);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return 0;
  }
  if (n <= 0) return 0;
  return (size_t)n > len ? len : (size_t)n;
}

// Forward declare.
emacs_value fzf_native_highlight_all(emacs_env *env,
                                     ptrdiff_t nargs,
                                     emacs_value args[],
                                     void *data_ptr);

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

  /* Empty query: don't score, but still strip stale `completions-common-part'
     face from the top-N candidates so backspacing to "" clears highlights left
     behind by a prior query.  Delegate to highlight-all, which respects
     `fussy-fzf-native-highlight' for the cap. */
  if (query.len == 0) {
    emacs_value hargs[2] = { args[0], args[1] };
    result = fzf_native_highlight_all(env, 2, hargs, NULL);
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

  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  fzf_pattern_t *pattern = fzf_parse_pattern(case_mode, false, query.b, true);
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

  /* Resolve C-side highlight cap from fussy-fzf-native-highlight.  After the
     sort, xs[0] is the highest-scoring candidate, so the top-N candidates
     are xs[0..hl_cap-1].  The original parsing pattern was already freed;
     re-parse for highlighting using the same case mode the scoring used. */
  size_t hl_cap = resolve_fussy_highlight_cap(env, len);
  fzf_pattern_t *hl_pattern = NULL;
  fzf_slab_t    *hl_slab    = NULL;
  if (hl_cap > 0) {
    hl_pattern = fzf_parse_pattern(case_mode, false, query.b, true);
    if (hl_pattern) hl_slab = fzf_make_default_slab();
    if (!hl_slab) {
      if (hl_pattern) { fzf_free_pattern(hl_pattern); hl_pattern = NULL; }
      hl_cap = 0;
    }
  }

  for (size_t i = len; i-- > 0;) {
    /* e.g. (put-text-property 0 1 'completion-score score x) */
    if (xs[i].s.len > 0) {
      env->funcall(env, Fput_text_property, 5,
                   (emacs_value[]) {
                     Qzero, Qone, Qcompletion_score,
                     env->make_integer(env, xs[i].score),
                     xs[i].value,
                   });
    }

    if (hl_pattern && i < hl_cap) {
      apply_highlight_positions(env, xs[i].s.b, hl_pattern, hl_slab,
                                xs[i].value);
    }

    result = env->funcall(env, Fcons, 2, (emacs_value[]) { xs[i].value, result });
  }

  if (hl_pattern) fzf_free_pattern(hl_pattern);
  if (hl_slab)    fzf_free_slab(hl_slab);

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

/* Strip `completions-common-part' face from STR_VAL without applying any new
   positions.  Used by the empty-query path of `fzf-native-highlight-all', and
   shares the (face nil) plist with `apply_highlight_positions'. */
static void clear_highlight_face(emacs_env *env, emacs_value str_val) {
  emacs_value len_v = env->funcall(env, Flength, 1, &str_val);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return;
  }
  emacs_value rargs[4] = { Qzero, len_v, Qface_nil_plist, str_val };
  env->funcall(env, Fremove_text_properties, 4, rargs);
  env->non_local_exit_clear(env);
}

// fzf-native-highlight-all COLLECTION QUERY
//
// Apply `completions-common-part' face to each candidate in COLLECTION
// against QUERY without scoring or sorting.  Intended for callers that
// already have a sorted result set but need to refresh stale highlights
// (e.g. fussy cache hits or the empty-query branch, where the C scoring
// path is skipped entirely and previously-applied face properties from
// a different query persist on the same Emacs string objects).
//
// When QUERY is empty, performs a clear-only pass: removes the face
// from the top-N candidates without computing new positions.  This is
// the path that fixes the "type m, backspace, highlight stays" case.
//
// Honors `fussy-fzf-native-highlight' the same way `fzf-native-score-all'
// does: nil → no-op, t → process all, N → process top N.  COLLECTION
// is assumed to be in display order (highest-scoring first).
//
// Returns COLLECTION unchanged.  Mutates the candidate strings in-place.
emacs_value fzf_native_highlight_all(emacs_env *env,
                                     ptrdiff_t UNUSED(nargs),
                                     emacs_value args[],
                                     void UNUSED(*data_ptr)) {
  struct Bump *bump = NULL;
  fzf_pattern_t *pattern = NULL;
  fzf_slab_t    *slab    = NULL;

  /* Treat an empty *or* undecodable query as clear-only.  The stale face
     properties live on the COLLECTION strings, not on the query, so we still
     need to walk the collection and strip face even if the query couldn't be
     coerced through `encode-coding-string'. */
  struct Str query = copy_emacs_string(env, &bump, args[1]);
  bool clear_only = (!query.b || query.len == 0);

  /* Accept both lists and vectors; mirror score-all's normalization so
     callers don't have to care which one they have on hand. */
  emacs_value collection = args[0];
  if (!env->eq(env, env->type_of(env, collection), env->intern(env, "vector"))) {
    collection = env->funcall(env, Fvconcat, 1, (emacs_value[]) { args[0] });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
      env->non_local_exit_clear(env);
      goto done;
    }
  }

  ptrdiff_t n = env->vec_size(env, collection);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    goto done;
  }

  /* Cap the highlight/clear pass to the user's `fussy-fzf-native-highlight'
     setting.  Returns 0 when highlighting is disabled — at that point the
     candidates can't have stale face from this module either, so skip. */
  size_t hl_cap = resolve_fussy_highlight_cap(env, (size_t)n);
  if (hl_cap == 0) goto done;

  if (!clear_only) {
    fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
    pattern = fzf_parse_pattern(case_mode, false, query.b, true);
    if (!pattern) goto done;
    slab = fzf_make_default_slab();
    if (!slab) goto done;
  }

  for (ptrdiff_t i = 0; i < (ptrdiff_t)hl_cap; i++) {
    emacs_value value = env->vec_get(env, collection, i);
    if (clear_only) {
      clear_highlight_face(env, value);
    } else {
      struct Str s = copy_emacs_string(env, &bump, value);
      if (!s.b) continue;
      apply_highlight_positions(env, s.b, pattern, slab, value);
    }
  }

done:
  if (slab)    fzf_free_slab(slab);
  if (pattern) fzf_free_pattern(pattern);
  bump_free(bump);
  /* Always return the original COLLECTION (not the vector-coerced copy)
     so list callers see their list back. */
  return args[0];
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
  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  fzf_pattern_t *pattern = fzf_parse_pattern(case_mode, false, query.b, true);
  if (!pattern) { goto err; }

  fzf_slab_t *slab;
  if (nargs > 2) {
    // Re-use SLAB argument.
    slab = env->get_user_ptr(env, args[2]);
  } else {
    // Create a one-time use slab.
    slab = fzf_make_default_slab();
  }

  int score = fzf_get_score(str.b, pattern, slab);

  /* Apply C-layer highlighting when fussy-fzf-native-highlight is non-nil
     and the candidate matched.  The cap concept does not apply to a single
     candidate — any non-nil value enables highlighting for this call. */
  if (score > 0 && resolve_fussy_highlight_cap(env, 1) > 0) {
    apply_highlight_positions(env, str.b, pattern, slab, args[0]);
  }

  /* Return (SCORE) — a single-element list.  Match indices are no longer
     surfaced to Elisp; highlighting is handled in C. */
  emacs_value score_val = env->make_integer(env, score);
  result = env->funcall(env, Flist, 1, &score_val);
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

#define ASYNC_INIT_CAP   4096
#define ASYNC_LINE_MAX   8192
#define ARENA_CHUNK_SIZE (4 * 1024 * 1024)  /* 4 MB per chunk */

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

typedef struct { char *str; int score; uint32_t idx; } ScoredStr;

/* Reference-counted immutable index array.  Allocated once by the scoring
   thread on cache_insert, retained in O(1) (atomic refcount bump under the
   cache mutex — no memcpy) by lookup consumers, and freed when the last
   consumer releases it.  Used to record the full set of matched candidate
   indices for a query, so a later subsuming query can refine-score that
   set + only the candidates that arrived since (delta scoring) instead of
   re-scanning the whole pool. */
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

/* LRU result cache.  Per-session, mutex-protected doubly-linked list with
   MRU at head and LRU at tail.  Each entry records:
     query    — the literal filter string (owned, strdup'd)
     pool_gen — s->count at the moment the entry was scored
     top      — copy of the top-K ScoredStr published to Elisp
     m_idx    — SharedIdx of all matched candidate indices (NULL for OR
                queries, which can never serve as refinement sources because
                adding an OR alternate widens the result set)

   Lookups happen on the Emacs main thread (dispatch); inserts happen on
   the scoring thread (publish).  Both serialize through cache->mu. */
typedef struct CacheEntry {
  struct CacheEntry *prev, *next;
  char           *query;
  size_t          pool_gen;
  ScoredStr      *top;
  size_t          top_count;
  SharedIdx      *m_idx;
  /* Parsed form of `query`, populated on insert.  NULL when parsing failed
     or for OR queries (which are excluded from prefix-refinement anyway).
     Owned by the entry; freed in cache_entry_free. */
  fzf_pattern_t  *parsed;
} CacheEntry;

typedef struct {
  pthread_mutex_t mu;
  CacheEntry     *head;     /* MRU */
  CacheEntry     *tail;     /* LRU */
  size_t          count;
  size_t          max_entries;
} Cache;

static void cache_init(Cache *c, size_t max_entries) {
  pthread_mutex_init(&c->mu, NULL);
  c->head = c->tail = NULL;
  c->count = 0;
  c->max_entries = max_entries ? max_entries : 40;
}

static void cache_entry_free(CacheEntry *e) {
  if (!e) return;
  free(e->query);
  free(e->top);
  shared_idx_release(e->m_idx);
  if (e->parsed) fzf_free_pattern(e->parsed);
  free(e);
}

static void cache_unlink_locked(Cache *c, CacheEntry *e) {
  if (e->prev) e->prev->next = e->next; else c->head = e->next;
  if (e->next) e->next->prev = e->prev; else c->tail = e->prev;
  e->prev = e->next = NULL;
  c->count--;
}

static void cache_push_head_locked(Cache *c, CacheEntry *e) {
  e->prev = NULL;
  e->next = c->head;
  if (c->head) c->head->prev = e;
  c->head = e;
  if (!c->tail) c->tail = e;
  c->count++;
}

static void cache_free(Cache *c) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *e = c->head;
  while (e) { CacheEntry *n = e->next; cache_entry_free(e); e = n; }
  c->head = c->tail = NULL;
  c->count = 0;
  pthread_mutex_unlock(&c->mu);
  pthread_mutex_destroy(&c->mu);
}

/* Q' subsumes Q iff neither contains '|' AND Q' is a byte-prefix of Q.
   Captures: extending a term (fo → foo), adding AND terms at end (fo → fo
   bar), adding negations/anchors at end (fo → fo !x).  Conservatively
   rejects every query containing '|' — adding an OR alternate widens
   results unpredictably.  This is the v1 rule, kept as a fast-path. */
static bool subsumes(const char *q_prime, const char *q) {
  if (strchr(q_prime, '|') || strchr(q, '|')) return false;
  size_t lp = strlen(q_prime);
  if (lp == 0) return true;
  size_t lq = strlen(q);
  if (lq < lp) return false;
  return memcmp(q, q_prime, lp) == 0;
}

/* Two parsed terms are equivalent iff they would match exactly the same
   strings: same algorithm, same negation flag, same case sensitivity, same
   text after fzf's prefix stripping.  Both terms must have been parsed with
   the same case mode (CaseIgnore in our usage), so `ptr` (the lowercased
   token) is directly comparable. */
static bool term_equiv(const fzf_term_t *a, const fzf_term_t *b) {
  if (a->fn != b->fn) return false;
  if (a->inv != b->inv) return false;
  if (a->case_sensitive != b->case_sensitive) return false;
  return strcmp(a->ptr, b->ptr) == 0;
}

/* P' subsumes P (term-set rule) iff every term-set in P' has an equivalent
   term-set in P.  In fzf's model, term-sets are AND'd together; adding
   more term-sets monotonically narrows the match set, so P (with all of
   P''s term-sets plus possibly more) matches a subset of P''s candidates.

   Restricted to non-OR queries: any term-set with >1 term is an OR (e.g.
   "fo | bar" parses as one set with two terms), and OR queries can never
   serve as refinement sources because adding an OR alternate widens the
   match set unpredictably.

   Catches v2-only cases: adding an AND term in non-prefix position (fo →
   x fo), term reordering (fo bar → bar fo), non-prefix negation (fo →
   !x fo).  Empty P' (zero term-sets) trivially subsumes anything. */
static bool subsumes_pattern(const fzf_pattern_t *p_prime,
                             const fzf_pattern_t *p) {
  if (!p_prime || !p) return false;
  /* Reject if either side has any OR-containing term-set. */
  for (size_t i = 0; i < p_prime->size; i++)
    if (p_prime->ptr[i]->size != 1) return false;
  for (size_t i = 0; i < p->size; i++)
    if (p->ptr[i]->size != 1) return false;
  /* Every single-term set in p_prime must equal some single-term set in p. */
  for (size_t i = 0; i < p_prime->size; i++) {
    fzf_term_t *t_prime = &p_prime->ptr[i]->ptr[0];
    bool found = false;
    for (size_t j = 0; j < p->size; j++) {
      if (term_equiv(t_prime, &p->ptr[j]->ptr[0])) { found = true; break; }
    }
    if (!found) return false;
  }
  return true;
}

/* Parse a query string into an fzf_pattern_t.  Returns NULL if the query
   is empty or parsing fails.  fzf_parse_pattern mutates its input, so we
   strdup first and free after — the returned pattern is self-contained. */
static fzf_pattern_t *parse_query_for_cache(const char *query,
                                            fzf_case_types case_mode) {
  if (!query || !*query) return NULL;
  char *dup = strdup(query);
  if (!dup) return NULL;
  fzf_pattern_t *p = fzf_parse_pattern(case_mode, false, dup, true);
  free(dup);
  return p;
}

/* Find an entry by exact query match.  Caller holds c->mu. */
static CacheEntry *cache_find_locked(Cache *c, const char *query) {
  for (CacheEntry *e = c->head; e; e = e->next)
    if (strcmp(e->query, query) == 0) return e;
  return NULL;
}

/* Exact lookup.  On hit, bumps entry to MRU and returns:
     *out_top, *out_top_count — caller-owned copy of the cached top-K
     *out_m_idx               — SharedIdx with refcount bumped (caller releases)
     *out_pool_gen            — pool size at the time this entry was scored
   Returns true on hit, false on miss. */
static bool cache_lookup_exact(Cache *c, const char *query,
                               ScoredStr **out_top, size_t *out_top_count,
                               SharedIdx **out_m_idx, size_t *out_pool_gen) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *e = cache_find_locked(c, query);
  if (!e) { pthread_mutex_unlock(&c->mu); return false; }

  ScoredStr *top_copy = NULL;
  if (e->top_count) {
    top_copy = malloc(e->top_count * sizeof *top_copy);
    if (top_copy) memcpy(top_copy, e->top, e->top_count * sizeof *top_copy);
  }
  *out_top       = top_copy;
  *out_top_count = top_copy ? e->top_count : 0;
  *out_m_idx     = shared_idx_retain(e->m_idx);
  *out_pool_gen  = e->pool_gen;

  /* Bump to MRU. */
  if (e != c->head) { cache_unlink_locked(c, e); cache_push_head_locked(c, e); }
  pthread_mutex_unlock(&c->mu);
  return true;
}

/* Prefix lookup: most-constrained Q' that subsumes Q (and is not Q itself).
   Uses byte-prefix OR term-set subsumption.  Skips entries with NULL m_idx
   (OR queries / empty match sets — can't serve as refinement sources).

   Best = the entry whose parsed pattern has the most terms.  More terms =
   more constraints = smaller match set = faster refinement scan.  Falls
   back to byte-prefix-length tiebreak when both have equal term counts
   (or for entries whose parsed pattern is unavailable). */
static bool cache_lookup_prefix(Cache *c, const char *query,
                                fzf_case_types case_mode,
                                ScoredStr **out_top, size_t *out_top_count,
                                SharedIdx **out_m_idx, size_t *out_pool_gen) {
  if (strchr(query, '|')) return false;   /* fast reject */

  fzf_pattern_t *p_query = parse_query_for_cache(query, case_mode);
  /* If parse failed and query isn't empty, fall back to byte-prefix only.
     Empty query has p_query == NULL but byte-prefix subsumes("", anything)
     also returns true so the loop still works. */

  pthread_mutex_lock(&c->mu);
  CacheEntry *best = NULL;
  size_t best_terms = 0;
  size_t best_len   = 0;
  for (CacheEntry *e = c->head; e; e = e->next) {
    if (!e->m_idx) continue;
    if (strcmp(e->query, query) == 0) continue;

    bool match = subsumes(e->query, query)
              || (p_query && subsumes_pattern(e->parsed, p_query));
    if (!match) continue;

    /* Term count = number of AND term-sets in the parsed pattern.  More
       sets = more constraints = smaller match set = better refinement
       source.  OR-containing entries have m_idx==NULL and were skipped. */
    size_t terms = e->parsed ? e->parsed->size : 0;
    size_t len   = strlen(e->query);
    if (terms > best_terms ||
        (terms == best_terms && len > best_len)) {
      best = e;
      best_terms = terms;
      best_len   = len;
    }
  }
  if (!best) {
    pthread_mutex_unlock(&c->mu);
    if (p_query) fzf_free_pattern(p_query);
    return false;
  }

  ScoredStr *top_copy = NULL;
  if (best->top_count) {
    top_copy = malloc(best->top_count * sizeof *top_copy);
    if (top_copy) memcpy(top_copy, best->top, best->top_count * sizeof *top_copy);
  }
  *out_top       = top_copy;
  *out_top_count = top_copy ? best->top_count : 0;
  *out_m_idx     = shared_idx_retain(best->m_idx);
  *out_pool_gen  = best->pool_gen;

  if (best != c->head) { cache_unlink_locked(c, best); cache_push_head_locked(c, best); }
  pthread_mutex_unlock(&c->mu);
  if (p_query) fzf_free_pattern(p_query);
  return true;
}

/* Insert or update an entry.  Performs all allocations BEFORE taking
   c->mu, so the critical section is just pointer swaps + LRU manipulation.
   Evicted entries are freed after the unlock.  m_idx may be NULL (OR queries
   or empty match sets); the entry is still inserted, but is then ineligible
   as a prefix-refinement source. */
static void cache_insert(Cache *c, const char *query, size_t pool_gen,
                         fzf_case_types case_mode,
                         const ScoredStr *top, size_t top_count,
                         const uint32_t *m_idx_src, size_t m_idx_count) {
  /* Pre-allocate everything outside the mutex. */
  char *q_dup = strdup(query);
  ScoredStr *top_dup = NULL;
  if (top_count && top) {
    top_dup = malloc(top_count * sizeof *top_dup);
    if (top_dup) memcpy(top_dup, top, top_count * sizeof *top_dup);
    else top_count = 0;
  }
  SharedIdx *sidx = (m_idx_src && m_idx_count && !strchr(query, '|'))
                    ? shared_idx_alloc(m_idx_src, m_idx_count) : NULL;
  /* Parse once on insert so cache_lookup_prefix doesn't pay parse cost on
     every iteration of its scan loop.  NULL is fine — entries with NULL
     parsed only participate via the byte-prefix subsumption fallback. */
  fzf_pattern_t *parsed = parse_query_for_cache(query, case_mode);

  if (!q_dup) {
    free(top_dup);
    shared_idx_release(sidx);
    if (parsed) fzf_free_pattern(parsed);
    return;
  }

  pthread_mutex_lock(&c->mu);
  CacheEntry *e = cache_find_locked(c, query);
  if (e) {
    /* Update existing entry: swap fields, release old refs after unlock. */
    char *old_q = e->query;
    ScoredStr *old_top = e->top;
    SharedIdx *old_idx = e->m_idx;
    fzf_pattern_t *old_parsed = e->parsed;
    e->query     = q_dup;
    e->top       = top_dup;
    e->top_count = top_dup ? top_count : 0;
    e->m_idx     = sidx;
    e->parsed    = parsed;
    e->pool_gen  = pool_gen;
    if (e != c->head) { cache_unlink_locked(c, e); cache_push_head_locked(c, e); }
    pthread_mutex_unlock(&c->mu);
    free(old_q);
    free(old_top);
    shared_idx_release(old_idx);
    if (old_parsed) fzf_free_pattern(old_parsed);
    return;
  }

  /* New entry. */
  CacheEntry *ne = calloc(1, sizeof *ne);
  if (!ne) {
    pthread_mutex_unlock(&c->mu);
    free(q_dup);
    free(top_dup);
    shared_idx_release(sidx);
    if (parsed) fzf_free_pattern(parsed);
    return;
  }
  ne->query     = q_dup;
  ne->top       = top_dup;
  ne->top_count = top_dup ? top_count : 0;
  ne->m_idx     = sidx;
  ne->parsed    = parsed;
  ne->pool_gen  = pool_gen;
  cache_push_head_locked(c, ne);

  /* Evict LRU if over capacity. */
  CacheEntry *evicted = NULL;
  if (c->count > c->max_entries && c->tail) {
    evicted = c->tail;
    cache_unlink_locked(c, evicted);
  }
  pthread_mutex_unlock(&c->mu);
  cache_entry_free(evicted);
}

typedef struct {
  pthread_t     reader;
  pid_t         pid;
  FILE         *fp;
  _Atomic bool stop;

  pthread_mutex_t mu;
  Arena           arena;   /* backing storage for all candidate strings */
  char          **cands;
  size_t          count;
  size_t          cap;
  _Atomic int     gen;

  size_t          last_filtered;   /* candidates matching last filter */
  size_t          last_total;      /* total candidates at last call */

  /* Background scoring thread */
  pthread_t        score_thread;
  pthread_mutex_t  score_req_mu;
  pthread_cond_t   score_req_cond;
  char            *score_req_filter;  /* owned; NULL = nothing pending */
  size_t           score_req_limit;
  fzf_case_types   score_req_case_mode;
  /* Refinement request: when score_req_refine_idx is non-NULL the next scoring
     run scores only those candidate indices plus s->cands[refine_delta_from..count].
     Ownership transfers to the scoring thread along with score_req_filter. */
  SharedIdx       *score_req_refine_idx;
  size_t           score_req_refine_delta_from;
  bool             score_req_stop;
  _Atomic bool     score_abort;       /* set to cancel in-flight workers */

  char            *score_current_filter; /* filter being actively scored (under score_req_mu) */
  size_t           score_current_limit;

  pthread_mutex_t  score_res_mu;
  ScoredStr       *score_results;     /* latest scored+sorted results */
  size_t           score_count;       /* number of entries in score_results */

  /* Result cache (LRU keyed by query, values include matched_idx for
     prefix refinement).  Read on dispatch (main thread); written on
     scoring publish (scoring thread). */
  Cache            cache;

  /* Read-only after session start; set from fzf-async-max-line-length defcustom.
     0 = no limit.  >0 = exclude lines longer than N chars.  <0 = truncate to |N|. */
  ptrdiff_t        max_line_length;
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

    ptrdiff_t mll = s->max_line_length;
    if (mll != 0) {
      ptrdiff_t cap = mll > 0 ? mll : -mll;
      if ((ptrdiff_t)len > cap) {
        if (mll > 0) continue;   /* exclude */
        len = (size_t)cap;       /* truncate */
        line[len] = '\0';
      }
    }

    char *dup = arena_strdup(&s->arena, line, len);
    if (!dup) continue;

    pthread_mutex_lock(&s->mu);
    if (s->count >= s->cap) {
      size_t  ncap = s->cap * 2;
      fzf_log("async_reader: reallocating candidates %zu -> %zu\n", s->cap, ncap);
      char  **nc   = realloc(s->cands, ncap * sizeof *nc);
      if (!nc) { free(dup); pthread_mutex_unlock(&s->mu); continue; }
      s->cands = nc;
      s->cap   = ncap;
    }
    s->cands[s->count++] = dup;
    pthread_mutex_unlock(&s->mu);
    atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
  }
  fzf_log("async_reader EXIT: total=%zu gen=%d\n",
          s->count, (int)atomic_load_explicit(&s->gen, memory_order_relaxed));
  return NULL;
}

static void *scoring_thread_fn(void *arg);  /* defined after async_scoring_worker */

static void async_session_destroy(void *ptr) {
  AsyncSession *s = ptr;
  if (!s) return;
  fzf_log("async_session_destroy: pid=%d count=%zu\n", (int)s->pid, s->count);

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
  s->score_req_stop   = true;
  pthread_cond_signal(&s->score_req_cond);
  pthread_mutex_unlock(&s->score_req_mu);
  pthread_join(s->score_thread, NULL);

  free(s->score_results);
  free(s->score_current_filter);
  cache_free(&s->cache);
  pthread_mutex_destroy(&s->score_res_mu);
  pthread_mutex_destroy(&s->score_req_mu);
  pthread_cond_destroy(&s->score_req_cond);

  /* Reader has been winding down since SIGTERM above; join it now. */
  pthread_join(s->reader, NULL);
  if (s->fp)      { fclose(s->fp); s->fp = NULL; }
  if (s->pid > 0) { waitpid(s->pid, NULL, 0); s->pid = -1; }
  pthread_mutex_lock(&s->mu);
  arena_free(&s->arena);
  free(s->cands);
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

  /* Use shell-file-name / shell-command-switch so behaviour matches
     shell-command (M-!) rather than hardcoding /bin/sh -c. */
  char *shell_prog = NULL, *shell_switch = NULL;
  {
    emacs_value sym = env->intern(env, "shell-file-name");
    emacs_value v   = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return &&
        !env->eq(env, v, Qnil)) {
      ptrdiff_t slen = 0;
      env->copy_string_contents(env, v, NULL, &slen);
      if (slen > 1) {
        shell_prog = malloc((size_t)slen);
        if (shell_prog) env->copy_string_contents(env, v, shell_prog, &slen);
      }
    } else {
      env->non_local_exit_clear(env);
    }
    if (!shell_prog) shell_prog = strdup("/bin/sh");
  }
  {
    emacs_value sym = env->intern(env, "shell-command-switch");
    emacs_value v   = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return &&
        !env->eq(env, v, Qnil)) {
      ptrdiff_t slen = 0;
      env->copy_string_contents(env, v, NULL, &slen);
      if (slen > 1) {
        shell_switch = malloc((size_t)slen);
        if (shell_switch) env->copy_string_contents(env, v, shell_switch, &slen);
      }
    } else {
      env->non_local_exit_clear(env);
    }
    if (!shell_switch) shell_switch = strdup("-c");
  }

  /* Build PATH from exec-path so the child shell can find binaries that
     Emacs can find, even on macOS GUI launches with a minimal inherited PATH. */
  char *exec_path_str = NULL;
  {
    emacs_value sym    = env->intern(env, "exec-path");
    emacs_value v      = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
    emacs_value sep    = env->make_string(env, ":", 1);
    emacs_value id     = env->intern(env, "identity");
    emacs_value mc_fn  = env->intern(env, "mapconcat");
    emacs_value mc_args[3] = {id, v, sep};
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
      emacs_value joined = env->funcall(env, mc_fn, 3, mc_args);
      if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
        ptrdiff_t plen = 0;
        env->copy_string_contents(env, joined, NULL, &plen);
        if (plen > 1) {
          exec_path_str = malloc((size_t)plen);
          if (exec_path_str)
            env->copy_string_contents(env, joined, exec_path_str, &plen);
        }
      }
    }
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
  }

  fzf_log("async_start: shell='%s' switch='%s' cmd='%s' dir='%s' PATH='%s'\n",
          shell_prog, shell_switch, cmd, dir ? dir : "(nil)",
          exec_path_str ? exec_path_str : "(inherited)");

  int pfd[2];
  if (pipe(pfd) != 0) {
    fzf_log("async_start: pipe failed\n");
    free(cmd);
    free(dir);
    free(shell_prog);
    free(shell_switch);
    free(exec_path_str);
    return Qnil;
  }

  pid_t pid = fork();
  if (pid < 0) {
    fzf_log("async_start: fork failed\n");
    close(pfd[0]);
    close(pfd[1]);
    free(cmd);
    free(dir);
    free(shell_prog);
    free(shell_switch);
    free(exec_path_str);
    return Qnil;
  }

  if (pid == 0) {
    close(pfd[0]);
    dup2(pfd[1], STDOUT_FILENO);
    close(pfd[1]);
    int dn = open("/dev/null", O_WRONLY);
    if (dn >= 0) { dup2(dn, STDERR_FILENO); close(dn); }
    if (exec_path_str) {
      const char *old = getenv("PATH");
      if (old && *old) {
        size_t nlen = strlen(exec_path_str) + 1 + strlen(old) + 1;
        char *new_path = malloc(nlen);
        if (new_path) {
          snprintf(new_path, nlen, "%s:%s", exec_path_str, old);
          setenv("PATH", new_path, 1);
          free(new_path);
        }
      } else {
        setenv("PATH", exec_path_str, 1);
      }
    }
    if (dir) chdir(dir);
    execl(shell_prog, shell_prog, shell_switch, cmd, (char *)NULL);
    _exit(127);
  }
  close(pfd[1]);
  free(shell_prog);
  free(shell_switch);
  free(exec_path_str);

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
  s->cap   = ASYNC_INIT_CAP;
  s->cands = malloc(s->cap * sizeof *s->cands);
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  atomic_store(&s->gen, 0);
  atomic_store(&s->score_abort, false);

  {
    emacs_value sym = env->intern(env, "fzf-async-max-line-length");
    emacs_value val = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    else if (env->eq(env, val, Qt))
      s->max_line_length = 512;
    else if (!env->eq(env, val, Qnil)) {
      s->max_line_length = (ptrdiff_t)env->extract_integer(env, val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        s->max_line_length = 0;
      }
    }
  }

  {
    size_t cache_max = 40;
    emacs_value sym = env->intern(env, "fzf-async-cache-size");
    emacs_value val = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    else if (!env->eq(env, val, Qnil)) {
      intmax_t n = env->extract_integer(env, val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n > 0)
        cache_max = (size_t)n;
    }
    cache_init(&s->cache, cache_max);
  }

  if (!s->fp || !s->cands ||
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
    fzf_log("async_stop: pid=%d total=%zu\n", (int)s->pid, s->count);
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

struct AsyncScoringBatch {
  unsigned len;
  ScoredStr xs[BATCH_SIZE];
};

struct AsyncScoringShared {
  fzf_pattern_t            *pattern;
  struct AsyncScoringBatch *batches;
  _Atomic ssize_t           remaining;
  _Atomic bool             *stop;     /* points to session's score_abort */
};

static void *async_scoring_worker(void *ptr) {
  struct AsyncScoringShared *shared = ptr;
  fzf_slab_t    *slab         = fzf_make_default_slab();
  fzf_pattern_t *pattern      = shared->pattern;

  ssize_t bi;
  while ((bi = atomic_fetch_sub_explicit(&shared->remaining, 1,
                                         memory_order_seq_cst) - 1) >= 0) {
    if (shared->stop && atomic_load_explicit(shared->stop, memory_order_relaxed))
      break;
    struct AsyncScoringBatch *batch = shared->batches + bi;
    unsigned n = 0;
    bool aborted = false;
    for (unsigned i = 0; i < batch->len; i++) {
      if ((i & 0xFF) == 0 && shared->stop &&
          atomic_load_explicit(shared->stop, memory_order_relaxed)) {
        aborted = true; break;
      }
      int sc = pattern ? fzf_get_score(batch->xs[i].str, pattern, slab) : 1;
      if (!pattern || sc > 0) {
        batch->xs[n]         = batch->xs[i];
        batch->xs[n++].score = sc;
      }
    }
    if (aborted) break;
    batch->len = n;
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
    char           *filter           = s->score_req_filter;       /* steal ownership */
    size_t          limit            = s->score_req_limit;
    fzf_case_types  case_mode        = s->score_req_case_mode;
    SharedIdx      *refine_idx       = s->score_req_refine_idx;   /* steal */
    size_t          refine_delta_from = s->score_req_refine_delta_from;
    s->score_req_filter      = NULL;
    s->score_req_refine_idx  = NULL;
    /* Record what we're about to score so main thread can skip abort for same filter */
    free(s->score_current_filter);
    s->score_current_filter = strdup(filter);
    s->score_current_limit  = limit;
    pthread_mutex_unlock(&s->score_req_mu);

    /* Reset abort flag AFTER stealing request so we don't race with the
       next dispatch that may have already set it again. */
    atomic_store_explicit(&s->score_abort, false, memory_order_seq_cst);

    /* Snapshot candidate count first (brief lock), then malloc outside lock,
       then memcpy under lock.  Keeps s->mu held only for the fast memcpy,
       not for the potentially-slow malloc with tens of millions of candidates. */
    pthread_mutex_lock(&s->mu);
    size_t count = s->count;
    pthread_mutex_unlock(&s->mu);

    /* Determine snap layout.  In full mode, snap[i] = s->cands[i] for
       i in [0, count); snap_count = count.  In refine mode, snap is the
       union of refine_idx (the previous match set) and the delta range
       s->cands[refine_delta_from .. count) — always a strict subset of
       the full pool, hence the speedup. */
    bool   refine    = (refine_idx != NULL && refine_delta_from <= count);
    size_t delta_len = refine ? (count - refine_delta_from) : 0;
    size_t snap_count = refine ? (refine_idx->count + delta_len) : count;

    char     **snap     = snap_count ? malloc(snap_count * sizeof *snap)     : NULL;
    uint32_t  *snap_idx = snap_count ? malloc(snap_count * sizeof *snap_idx) : NULL;
    if (snap_count && (!snap || !snap_idx)) {
      pthread_mutex_lock(&s->score_req_mu);
      free(s->score_current_filter); s->score_current_filter = NULL;
      pthread_mutex_unlock(&s->score_req_mu);
      free(snap); free(snap_idx); free(filter); shared_idx_release(refine_idx); continue;
    }

    pthread_mutex_lock(&s->mu);
    if (s->count < count) count = s->count;   /* cap if reader shrank (shouldn't happen) */
    if (refine) {
      /* Cap delta range too in case reader shrank. */
      if (refine_delta_from > count) refine_delta_from = count;
      delta_len = count - refine_delta_from;
      /* Refine entries first: validate each refine_idx[i] < count. */
      size_t w = 0;
      for (size_t i = 0; i < refine_idx->count; i++) {
        uint32_t gi = refine_idx->idx[i];
        if (gi < count) { snap[w] = s->cands[gi]; snap_idx[w] = gi; w++; }
      }
      /* Delta entries. */
      for (size_t k = 0; k < delta_len; k++) {
        size_t gi = refine_delta_from + k;
        snap[w] = s->cands[gi]; snap_idx[w] = (uint32_t)gi; w++;
      }
      snap_count = w;
    } else if (snap) {
      memcpy(snap, s->cands, count * sizeof *snap);
      for (size_t i = 0; i < count; i++) snap_idx[i] = (uint32_t)i;
      snap_count = count;
    }
    pthread_mutex_unlock(&s->mu);

    /* Batch; check abort every 64 K items so a filter change is noticed quickly. */
    struct AsyncScoringBatch *batches = NULL;
    size_t bi = 0, bcap = 0;
    bool batch_ok = true;
    for (size_t i = 0; i < snap_count; i++) {
      if ((i & 0xFFFF) == 0 &&
          atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
        batch_ok = false; break;
      }
      if (!batches || (batches[bi].len >= BATCH_SIZE && ++bi >= bcap)) {
        bcap = bcap ? bcap * 2 : 1;
        struct AsyncScoringBatch *nb = realloc(batches, bcap * sizeof *nb);
        if (!nb) { batch_ok = false; break; }
        for (size_t k = bi; k < bcap; k++) nb[k].len = 0;
        batches = nb;
      }
      batches[bi].xs[batches[bi].len].str   = snap[i];
      batches[bi].xs[batches[bi].len].score = 0;
      batches[bi].xs[batches[bi].len].idx   = snap_idx[i];
      batches[bi].len++;
    }
    if (!batch_ok) {
      pthread_mutex_lock(&s->score_req_mu);
      free(s->score_current_filter); s->score_current_filter = NULL;
      pthread_mutex_unlock(&s->score_req_mu);
      free(snap); free(snap_idx); free(filter); free(batches);
      shared_idx_release(refine_idx); continue;
    }

    unsigned num_batches = snap_count ? (unsigned)(bi + 1) : 0;
    unsigned max_workers = (unsigned)sysconf(_SC_NPROCESSORS_ONLN);

    size_t flen = strlen(filter);
    fzf_pattern_t *pattern = flen ? fzf_parse_pattern(case_mode, false, filter, true) : NULL;
    bool has_pattern = (pattern != NULL);

    struct AsyncScoringShared shared = {
      .pattern   = pattern,
      .batches   = batches,
      .remaining = num_batches,
      .stop      = &s->score_abort,
    };

    pthread_t *threads    = malloc(max_workers * sizeof *threads);
    unsigned   num_workers = 0;
    if (threads && num_batches) {
      for (; num_workers < MIN(max_workers, num_batches); num_workers++)
        pthread_create(threads + num_workers, NULL, async_scoring_worker, &shared);
    }
    for (unsigned i = 0; i < num_workers; i++)
      pthread_join(threads[i], NULL);
    free(threads);
    if (pattern) fzf_free_pattern(pattern);

    /* If a different filter arrived while we were scoring, discard partial results. */
    if (atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
      pthread_mutex_lock(&s->score_req_mu);
      free(s->score_current_filter); s->score_current_filter = NULL;
      pthread_mutex_unlock(&s->score_req_mu);
      free(snap); free(snap_idx); free(batches); free(filter);
      shared_idx_release(refine_idx); continue;
    }

    /* Compact into flat array */
    size_t total = 0;
    for (unsigned i = 0; i < num_batches; i++) total += batches[i].len;

    ScoredStr *flat = total ? malloc(total * sizeof *flat) : NULL;
    size_t pos = 0;
    if (flat) {
      for (unsigned i = 0; i < num_batches; i++) {
        struct AsyncScoringBatch *b = batches + i;
        for (unsigned j = 0; j < b->len; j++)
          flat[pos++] = b->xs[j];
      }
      if (has_pattern && pos > 1)
        counting_sort_scored(flat, pos);
    }

    size_t emit = (limit && limit < pos) ? limit : pos;

    /* Build matched_idx array (all pos matches, not just top-K) for the
       cache so a future subsuming query can refine-score only this set. */
    uint32_t *m_idx_buf = (pos && flat) ? malloc(pos * sizeof *m_idx_buf) : NULL;
    if (m_idx_buf) for (size_t k = 0; k < pos; k++) m_idx_buf[k] = flat[k].idx;

    /* Cache the result.  pool_gen = count (the pool size we actually scored).
       For refine runs, count may be > refine_delta_from, so the new entry
       supersedes the old one as a refinement source for the same query. */
    cache_insert(&s->cache, filter, count, case_mode, flat, emit, m_idx_buf, pos);
    free(m_idx_buf);

    /* Clear active-filter marker before publishing results */
    pthread_mutex_lock(&s->score_req_mu);
    free(s->score_current_filter); s->score_current_filter = NULL;
    pthread_mutex_unlock(&s->score_req_mu);

    pthread_mutex_lock(&s->score_res_mu);
    free(s->score_results);
    s->score_results = flat;
    s->score_count   = emit;
    s->last_filtered = pos;
    s->last_total    = count;
    pthread_mutex_unlock(&s->score_res_mu);

    /* Increment gen so Elisp knows new results are available */
    atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);

    fzf_log("scoring_thread: filter='%s' filtered=%zu total=%zu emit=%zu refine=%d scanned=%zu\n",
            filter, pos, count, emit, refine ? 1 : 0, snap_count);

    free(snap); free(snap_idx); free(batches); free(filter);
    shared_idx_release(refine_idx);
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
  /* Keep a copy for C-side highlighting; filter ownership may transfer below. */
  char *filter_for_hilit = (flen > 1) ? strdup(filter) : NULL;

  size_t limit = 0;
  if (nargs > 2 && !env->eq(env, args[2], Qnil))
    limit = (size_t)env->extract_integer(env, args[2]);

  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);

  /* Cache lookup.  Exact-fresh hits skip scoring entirely; exact-stale
     and prefix hits dispatch a refinement scoring run that scans only
     the prior match set + candidates that arrived since.  Misses fall
     through to a normal full-pool scoring run. */
  ScoredStr *cached_top    = NULL;
  size_t     cached_count  = 0;
  SharedIdx *cached_m_idx  = NULL;
  size_t     cached_pool_gen = 0;

  bool exact_hit = cache_lookup_exact(&s->cache, filter,
                                      &cached_top, &cached_count,
                                      &cached_m_idx, &cached_pool_gen);
  bool prefix_hit = false;
  if (!exact_hit)
    prefix_hit = cache_lookup_prefix(&s->cache, filter, case_mode,
                                     &cached_top, &cached_count,
                                     &cached_m_idx, &cached_pool_gen);

  pthread_mutex_lock(&s->mu);
  size_t current_pool = s->count;
  pthread_mutex_unlock(&s->mu);

  bool exact_fresh = exact_hit && (cached_pool_gen == current_pool);

  fzf_log("async_candidates: filter='%s' limit=%zu pool=%zu hit=%s%s%s\n",
          filter, limit, current_pool,
          exact_fresh  ? "exact-fresh" : "",
          (exact_hit && !exact_fresh) ? "exact-stale" : "",
          prefix_hit   ? "prefix" : (!exact_hit ? "miss" : ""));

  /* Enqueue the new request.  Only abort in-flight scoring if the filter
     actually changed — same-filter timer re-triggers must not interrupt a
     scoring run that is still working on the same query, which would cause
     a livelock where scoring never completes on large candidate sets.

     On exact-fresh hit we skip enqueueing entirely — no work needed. */
  pthread_mutex_lock(&s->score_req_mu);
  bool filter_changed = !(s->score_current_filter &&
                          strcmp(s->score_current_filter, filter) == 0 &&
                          s->score_current_limit == limit);
  if (filter_changed)
    atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);

  if (exact_fresh) {
    /* No work to dispatch; release locally-owned filter + retained idx. */
    free(filter);
    shared_idx_release(cached_m_idx);
  } else {
    free(s->score_req_filter);
    shared_idx_release(s->score_req_refine_idx);
    s->score_req_filter            = filter;          /* transfer */
    s->score_req_limit             = limit;
    s->score_req_case_mode         = case_mode;
    s->score_req_refine_idx        = cached_m_idx;    /* transfer (NULL on miss) */
    s->score_req_refine_delta_from = cached_pool_gen;
    pthread_cond_signal(&s->score_req_cond);
  }
  pthread_mutex_unlock(&s->score_req_mu);

  /* Determine display set.  Cache hits return their (stale-but-useful)
     top-K immediately; misses fall back to whatever the scorer last
     published. */
  ScoredStr *snap   = NULL;
  size_t     rcount = 0;
  if (exact_hit || prefix_hit) {
    snap   = cached_top;     /* ownership transferred from cache_lookup_*. */
    rcount = cached_count;
  } else {
    pthread_mutex_lock(&s->score_res_mu);
    rcount = s->score_count;
    snap   = rcount ? malloc(rcount * sizeof *snap) : NULL;
    if (snap && s->score_results)
      memcpy(snap, s->score_results, rcount * sizeof *snap);
    else
      rcount = 0;
    pthread_mutex_unlock(&s->score_res_mu);
  }

  /* Resolve C-side highlight cap from defcustoms fzf-async-highlight and
     fzf-async-highlight-max-candidates.  Both are read via symbol-value so
     the user can change them without reloading the module. */
  size_t         hl_cap     = 0;
  fzf_pattern_t *hl_pattern = NULL;
  fzf_slab_t    *hl_slab    = NULL;

  if (filter_for_hilit) {
    emacs_value sym_hi = env->intern(env, "fzf-async-highlight");
    emacs_value hi     = env->funcall(env, env->intern(env, "symbol-value"), 1, &sym_hi);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    else if (env->eq(env, hi, Qt))
      hl_cap = rcount;                   /* t → highlight everything */
    else if (!env->eq(env, hi, Qnil)) { /* integer → highlight top N */
      intmax_t n = env->extract_integer(env, hi);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n > 0)
        hl_cap = (size_t)n;
    }
    /* nil or negative integer → hl_cap stays 0, no highlighting */
  }
  if (hl_cap > 0) {
    hl_pattern = fzf_parse_pattern(case_mode, false, filter_for_hilit, true);
    hl_slab    = fzf_make_default_slab();
  }

  /* Build Emacs list from stale results — strings are stable until session destroy.
     snap[0] is the highest-scoring candidate (prepend loop puts it at list head).
     Apply fzf_get_positions highlighting for snap[0..hl_cap-1] (i < hl_cap).
     NOTE: fzf positions are byte offsets; put-text-property uses character
     positions.  For ASCII candidates these are identical.  Multi-byte UTF-8
     candidates may have slightly misaligned highlights — acceptable for now. */
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

    if (hl_pattern && i < hl_cap) {
      fzf_position_t *pos = fzf_get_positions(snap[i].str, hl_pattern, hl_slab);
      if (pos && pos->size > 0) {
        /* pos->data[] is in descending order: pos->data[0] = highest position.
           Iterate ascending (j from size-1 to 0) to find contiguous runs. */
        size_t plen      = pos->size;
        size_t run_start = pos->data[plen - 1];
        size_t run_end   = run_start;
        for (ptrdiff_t j = (ptrdiff_t)plen - 2; j >= 0; j--) {
          size_t p = pos->data[j];
          if (p == run_end + 1) {
            run_end = p;
          } else {
            emacs_value a[5] = {
              env->make_integer(env, (intmax_t)run_start),
              env->make_integer(env, (intmax_t)(run_end + 1)),
              Qface, Qcompletions_common_part, str };
            env->funcall(env, Fput_text_property, 5, a);
            env->non_local_exit_clear(env);
            run_start = run_end = p;
          }
        }
        emacs_value a[5] = {
          env->make_integer(env, (intmax_t)run_start),
          env->make_integer(env, (intmax_t)(run_end + 1)),
          Qface, Qcompletions_common_part, str };
        env->funcall(env, Fput_text_property, 5, a);
        env->non_local_exit_clear(env);
      }
      fzf_free_positions(pos);
    }

    result = env->funcall(env, Fcons, 2, (emacs_value[]){ str, result });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      break;
  }

  if (hl_pattern) fzf_free_pattern(hl_pattern);
  if (hl_slab)    fzf_free_slab(hl_slab);
  free(filter_for_hilit);
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

  // fzf-native-highlight-all COLLECTION QUERY
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-highlight-all"),
      env->make_function(env, 2, 2, fzf_native_highlight_all,
                         "Apply fzf match highlights to COLLECTION against QUERY.\n"
                         "Mutates each candidate string's text properties in place;\n"
                         "stale `completions-common-part' face from a prior query is\n"
                         "stripped before new positions are applied.  No scoring or\n"
                         "sorting is performed.\n"
                         "\n"
                         "\\(fn COLLECTION QUERY)",
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
  Qface = env->make_global_ref(env, env->intern(env, "face"));
  Qcompletions_common_part = env->make_global_ref(env, env->intern(env, "completions-common-part"));
  Fremove_text_properties = env->make_global_ref(env, env->intern(env, "remove-text-properties"));
  /* Pre-built (face nil) plist passed to remove-text-properties to strip the
     `face' property regardless of value.  Built once to avoid allocating a
     fresh cons cell on every highlight call. */
  Qface_nil_plist = env->make_global_ref(
    env, env->funcall(env, Flist, 2, (emacs_value[]){Qface, Qnil}));
  Qutf_8 = env->make_global_ref(env, env->intern(env, "utf-8"));
  Qlistofzero = env->make_global_ref(
    env, env->funcall(env, Fcons, 2,
                      (emacs_value[]){env->make_integer(env, 0), Qnil}));
  Qzero = env->make_global_ref(env, env->make_integer(env, 0));
  Qone = env->make_global_ref(env, env->make_integer(env, 1));

  return 0;
}
