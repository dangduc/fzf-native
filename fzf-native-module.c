/* SPDX-License-Identifier: GPL-3.0-or-later */
/* strdup is POSIX (not C11); Linux glibc hides it under c11 without this. */
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
#include "fzf-additions.h"
#include "utf8proc-2.10.0/utf8proc.h"
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
#include <errno.h>

/* Block all signals on the current thread.  Worker threads call this on
   entry so async signals (SIGCHLD, SIGIO, ...) only ever land on Emacs's
   main thread.  Otherwise Emacs's signal handler forwards via pthread_kill,
   which can recursively lock an os_unfair_lock if it fires while the worker
   is inside libsystem code — observed crash on macOS.  Synchronous faults
   (SIGSEGV/etc.) are delivered by the kernel regardless of mask. */
static inline void fzf_block_all_signals(void) {
  sigset_t s;
  sigfillset(&s);
  pthread_sigmask(SIG_BLOCK, &s, NULL);
}
#else
/* Non-POSIX (Windows): no signals to block, worker entry calls become no-ops. */
static inline void fzf_block_all_signals(void) {}
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
#define ASYNC_WORKER_LIMIT 64
#define BATCH_CACHE_BUCKETS 4096
#define BATCH_CACHE_SPARSE_LIMIT 128

EXPORT
int plugin_is_GPL_compatible;

emacs_value Qnil, Qlistofzero, Fcons, Flist, Qt;
emacs_value Fhashtablep, Fmessage, Fvectorp, Fconsp, Fcdr, Fcar, Fvconcat;
emacs_value Ffunctionp, Fsymbolp, Fsymbolname, Flength, Fnth, Fprinc, Freverse;
emacs_value Qcompletion_score, Fput_text_property, Qzero, Qone;
emacs_value Fcopy_sequence, Fsetcar, Faset;
emacs_value Fencode_coding_string, Qutf_8;
emacs_value Qface, Qcompletions_common_part;
emacs_value Fremove_text_properties, Qface_nil_plist;
emacs_value Fsymbol_value;
/* Cached defcustom name symbols — interned once at init, looked up via
   `defcustom_value' on each read.  The values themselves stay dynamic
   so user `setq' / `customize-set-variable' is respected. */
emacs_value Qsym_case_mode, Qsym_fuzzy, Qsym_batch_highlight, Qsym_async_highlight;
emacs_value Qsym_max_line_length, Qsym_async_cache_size;
emacs_value Qsym_async_batch_cache_bytes, Qsym_filter_only_min_pool;
emacs_value Qsym_filter_only_length, Qsym_filter_only_logic;
emacs_value Qsym_shell_file_name, Qsym_shell_command_switch, Qsym_exec_path;
emacs_value Qsym_highlight_fn;
/* Cached value symbols for `type-of' comparisons and signal/error names. */
emacs_value Qvector, Qstring, Qignore, Qrespect;
emacs_value Qor, Qand;
emacs_value Qstringp, Qwrong_type_argument, Qerror;


/** An Emacs string made accessible by copying. */
struct Str { char *b; size_t len; };

/* Count Emacs-style characters (Unicode codepoints) in UTF-8 data.  Invalid
   bytes count as one character each so byte-junk inputs always make progress
   and retain the module's existing best-effort behavior. */
static size_t utf8_character_count(const char *str, size_t byte_len) {
  size_t byte_pos = 0;
  size_t char_count = 0;

  while (byte_pos < byte_len) {
    utf8proc_int32_t codepoint;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)(str + byte_pos),
        (utf8proc_ssize_t)(byte_len - byte_pos), &codepoint);
    byte_pos += width > 0 ? (size_t)width : 1;
    char_count++;
  }
  return char_count;
}

/* Return the byte length of the first CHAR_LIMIT codepoints without splitting
   a valid UTF-8 sequence.  Invalid bytes count as one codepoint. */
static size_t utf8_prefix_byte_length(const char *str, size_t byte_len,
                                      size_t char_limit) {
  size_t byte_pos = 0;
  size_t char_count = 0;

  while (byte_pos < byte_len && char_count < char_limit) {
    utf8proc_int32_t codepoint;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)(str + byte_pos),
        (utf8proc_ssize_t)(byte_len - byte_pos), &codepoint);
    byte_pos += width > 0 ? (size_t)width : 1;
    char_count++;
  }
  return byte_pos;
}

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

  if (!env->copy_string_contents(env, value, buf = new->cursor, &len)) {
    /* Re-signal on the retry (e.g. unicode-string-p, or len shrunk between
       calls): clear the pending exit so the caller can try the
       encode-coding-string fallback, and drop this candidate. Without this
       the signal would ride out on a "successful" Str and surface later from
       deep inside Fapply / the byte-code interpreter. */
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    return (struct Str) { 0 };
  }
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

static void insertion_sort_candidates(struct Candidate *xs, size_t n) {
  for (size_t i = 1; i < n; i++) {
    struct Candidate candidate = xs[i];
    size_t j = i;
    while (j > 0 && xs[j - 1].score < candidate.score) {
      xs[j] = xs[j - 1];
      j--;
    }
    xs[j] = candidate;
  }
}

/* Counting sort of xs[0..n-1] by score, descending.
   O(n + max_score). Falls back to qsort if allocations fail.
   The normal insertion/counting-sort paths are stable; the emergency qsort
   fallback after an allocation failure may lose same-score input order.
   Caller must ensure every xs[i].score >= 0; negative scores would
   index count[] out of bounds (undefined behavior). */
static void counting_sort_candidates(struct Candidate *xs, size_t n) {
  if (n <= 1) return;
  /* Avoid the counting-sort allocations for tiny inputs, but retain the
     stable input-order tie-break promised by the large-input path. */
  if (n < 64) { insertion_sort_candidates(xs, n); return; }
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
  /* When true, workers call `fzf_has_match' (cheap boolean) instead of
     `fzf_get_score' and assign score=1 to survivors.  Caller is expected
     to skip the counting sort so input order is preserved. */
  bool filter_only;
};

// Most of the threading lifted from https://github.com/axelf4/hotfuzz
static void *worker_routine(void *ptr) {
  fzf_block_all_signals();
  /* printf("-----\nStarting Worker Routine\n-----\n"); */
  // Create a one-time use slab.
  fzf_slab_t *slab = fzf_make_default_slab();

  struct Shared *shared = ptr;
  fzf_pattern_t *pattern = shared->pattern;
  bool filter_only = shared->filter_only;
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
        int score = filter_only
          ? (fzf_has_match(x.s.b, pattern, slab) ? 1 : 0)
          : fzf_get_score(x.s.b, pattern, slab);
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

/* Per-call scratch for `dispatch_highlight_runs'.  Sized at the start of a
   score / highlight-all / async-candidates call to the maximum possible
   position count (the query length); reused across every top-N candidate
   in that call to avoid per-candidate malloc/free churn.

   `runs` is split into two halves: starts in [0, capacity), ends in
   [capacity, 2*capacity).  `vargs` holds 2*capacity emacs_values
   (alternating start-val/end-val) for the `vector' funcall. */
typedef struct {
  size_t *starts;
  size_t *ends;
  emacs_value *vargs;
  size_t capacity;  /* max positions; 0 means uninitialized */
} HlScratch;

static bool hl_scratch_init(HlScratch *s, size_t max_pos) {
  s->capacity = 0;
  s->starts = NULL; s->ends = NULL; s->vargs = NULL;
  if (max_pos == 0) return true;
  s->starts = (size_t *)malloc(max_pos * sizeof(size_t));
  s->ends   = (size_t *)malloc(max_pos * sizeof(size_t));
  s->vargs  = (emacs_value *)malloc(max_pos * 2 * sizeof(emacs_value));
  if (!s->starts || !s->ends || !s->vargs) {
    free(s->starts); free(s->ends); free(s->vargs);
    s->starts = NULL; s->ends = NULL; s->vargs = NULL;
    return false;
  }
  s->capacity = max_pos;
  return true;
}

static void hl_scratch_free(HlScratch *s) {
  free(s->starts);
  free(s->ends);
  free(s->vargs);
  s->starts = NULL; s->ends = NULL; s->vargs = NULL;
  s->capacity = 0;
}

/* NOTE: upstream main carried a `runs_byte_to_char' helper here that converted
   fzf's byte-offset positions to character offsets, because upstream fzf.c
   returns byte positions.  This fork's UTF-8 fzf.c instead returns character
   offsets directly (see `dispatch_highlight_runs'), so that conversion was
   removed to avoid double-converting multibyte candidates. */

/* Dispatch fzf positions on CSTR to HOOK as character-offset runs against
   STR.  POS->data[] is fzf's descending byte-offset list; consolidated
   into ascending contiguous runs, converted to char offsets, packed into
   [s0 e0 s1 e1 …] vector, and passed as (funcall HOOK STR positions).
   No-op if POS is NULL/empty or HOOK is nil.

   SCRATCH provides reusable buffers sized at the start of the score call;
   when NULL or undersized, falls back to a per-call malloc/free pair. */
static void dispatch_highlight_runs(emacs_env *env, const char *cstr,
                                    fzf_position_t *pos,
                                    emacs_value str, emacs_value hook,
                                    HlScratch *scratch) {
  if (!pos || pos->size == 0) return;
  if (env->eq(env, hook, Qnil)) return;

  size_t plen = pos->size;
  size_t *starts;
  size_t *ends;
  emacs_value *vargs;
  bool need_free = false;

  if (scratch && plen <= scratch->capacity) {
    starts = scratch->starts;
    ends   = scratch->ends;
    vargs  = scratch->vargs;
  } else {
    starts = (size_t *)malloc(plen * sizeof(size_t));
    ends   = (size_t *)malloc(plen * sizeof(size_t));
    vargs  = (emacs_value *)malloc(plen * 2 * sizeof(emacs_value));
    if (!starts || !ends || !vargs) {
      free(starts); free(ends); free(vargs); return;
    }
    need_free = true;
  }

  /* Group ascending positions (fzf emits descending; we walk j = plen-2..0
     against an ascending pos->data, with pos->data[plen-1] as the
     smallest seed) into contiguous [start, end+1) runs. */
  size_t n_runs = 0;
  size_t cs = pos->data[plen - 1];
  size_t ce = cs;
  for (ptrdiff_t j = (ptrdiff_t)plen - 2; j >= 0; j--) {
    size_t p = pos->data[j];
    if (p == ce + 1) { ce = p; continue; }
    starts[n_runs] = cs;
    ends[n_runs++] = ce + 1;
    cs = ce = p;
  }
  starts[n_runs] = cs;
  ends[n_runs++] = ce + 1;

  /* This fork's fzf.c (UTF-8 build) already emits *character* offsets, not
     byte offsets: its `_utf8' matching variants convert via `utf8_byte_to_char'
     before returning, and the ASCII path's byte offsets equal char offsets.
     So consume `pos->data' directly; running module.c's own byte->char pass
     here would double-convert and mis-highlight multibyte candidates. */
  (void)cstr;

  for (size_t i = 0; i < n_runs; ++i) {
    vargs[2 * i]     = env->make_integer(env, (intmax_t)starts[i]);
    vargs[2 * i + 1] = env->make_integer(env, (intmax_t)ends[i]);
  }

  emacs_value positions = env->funcall(env, Qvector,
                                       (ptrdiff_t)(n_runs * 2), vargs);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    if (need_free) { free(starts); free(ends); free(vargs); }
    return;
  }

  /* Errors from the user handler degrade to no-highlight for this
     candidate; they never abort the surrounding score call. */
  env->funcall(env, hook, 2, (emacs_value[]){ str, positions });
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
  }

  if (need_free) { free(starts); free(ends); free(vargs); }
}

/* Attempt `copy-sequence' on VAL so callers can apply face / text properties
   without polluting the caller's original Lisp string.  Returns the fresh
   copy on success.  Returns VAL itself on allocation failure (e.g., heap
   exhaustion inside the funcall) — graceful degradation: losing copy
   isolation for a single candidate is preferable to dropping it from the
   result and is rare enough in practice to be acceptable. */
static emacs_value try_copy_string(emacs_env *env, emacs_value val) {
  emacs_value cp = env->funcall(env, Fcopy_sequence, 1, &val);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return val;
  }
  return cp;
}

/* Apply match highlights to STR_VAL using HOOK.  Computes positions via
   fzf_get_positions, groups them into contiguous runs, converts byte→char
   offsets, and dispatches to HOOK as `(funcall HOOK STR_VAL POSITIONS)'
   — where POSITIONS is `[s0 e0 s1 e1 …]'.  HOOK owns all face mutation
   on STR_VAL.  Skipped when CSTR is empty or HOOK is nil.  SCRATCH is
   the per-call buffer pool; pass NULL for one-shot callers. */
static void apply_highlight_positions(emacs_env *env,
                                      const char *cstr,
                                      fzf_pattern_t *pattern,
                                      fzf_slab_t *slab,
                                      emacs_value str_val,
                                      emacs_value hook,
                                      HlScratch *scratch) {
  if (cstr[0] == '\0') return;
  if (env->eq(env, hook, Qnil)) return;
  fzf_position_t *pos = fzf_get_positions(cstr, pattern, slab);
  dispatch_highlight_runs(env, cstr, pos, str_val, hook, scratch);
  fzf_free_positions(pos);
}

/* Read SYM via `symbol-value' and return its value, or FALLBACK on any
   read failure (unbound, non-local exit, etc.).  Clears the pending
   non-local exit on failure so the caller can continue.  Centralizes
   the read-defcustom-with-fallback pattern used across this file. */
static emacs_value defcustom_value(emacs_env *env, emacs_value sym,
                                   emacs_value fallback) {
  emacs_value v = env->funcall(env, Fsymbol_value, 1, &sym);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return fallback;
  }
  return v;
}

/* Read `fzf-native-case-mode' via symbol-value and resolve to fzf_case_types.
   Recognized symbol values: smart (default), ignore, respect.
   Falls back to CaseSmart on any read or comparison failure. */
static fzf_case_types resolve_fzf_native_case_mode(emacs_env *env) {
  emacs_value v = defcustom_value(env, Qsym_case_mode, Qnil);
  if (env->eq(env, v, Qignore))  return CaseIgnore;
  if (env->eq(env, v, Qrespect)) return CaseRespect;
  return CaseSmart;
}

/* Read `fzf-native-fuzzy' via symbol-value and resolve to a bool.
   Returns false only for an explicit nil; defaults to true on any read
   failure so the historical fuzzy-on behaviour is preserved. */
static bool resolve_fzf_native_fuzzy(emacs_env *env) {
  emacs_value v = defcustom_value(env, Qsym_fuzzy, Qt);
  return !env->eq(env, v, Qnil);
}

/* Read fussy-fzf-native-highlight via symbol-value and resolve to a cap.
   Returns:
     0    — no highlighting (nil, negative, unreadable, or zero).
     LEN  — highlight all (t).
     N    — highlight top N (clamped to LEN). */
static size_t resolve_fussy_highlight_cap(emacs_env *env, size_t len) {
  /* Canonical name; fussy bridges its `fussy-fzf-native-highlight'
     onto this via `setq-local' inside its all-completions entry. */
  emacs_value v = defcustom_value(env, Qsym_batch_highlight, Qnil);
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

/* Resolve filter-only thresholds and logic from defcustoms.
   Writes the integer values back via out-params; returns true if `logic'
   is 'and (false = OR, the default).  A nil/0 threshold means the arm
   is disabled — callers should treat it as "this trigger never fires."

   Reads are independent and tolerant: bad reads default to disabled / OR. */
static bool resolve_filter_only_settings(emacs_env *env,
                                         size_t *out_min_pool,
                                         size_t *out_max_len) {
  *out_min_pool = 0;
  *out_max_len  = 0;

  emacs_value vp = defcustom_value(env, Qsym_filter_only_min_pool, Qnil);
  if (!env->eq(env, vp, Qnil)) {
    intmax_t n = env->extract_integer(env, vp);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    else if (n > 0)
      *out_min_pool = (size_t)n;
  }

  emacs_value vl = defcustom_value(env, Qsym_filter_only_length, Qnil);
  if (!env->eq(env, vl, Qnil)) {
    intmax_t n = env->extract_integer(env, vl);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    else if (n > 0)
      *out_max_len = (size_t)n;
  }

  emacs_value vlogic = defcustom_value(env, Qsym_filter_only_logic, Qor);
  return env->eq(env, vlogic, Qand);
}

/* Decide filter-only mode from already-resolved settings + the call's
   query_len and pool_size.  Encapsulates the OR/AND composition so both
   the sync and async paths agree on semantics:

     OR  — either enabled trigger firing is sufficient.
     AND — every enabled trigger must fire; disabled arms are ignored.

   If both thresholds are disabled (0) the result is always false. */
static bool decide_filter_only(size_t min_pool, size_t max_len,
                               bool logic_and,
                               size_t query_len, size_t pool_size) {
  if (min_pool == 0 && max_len == 0) return false;
  bool by_pool = (min_pool > 0) && (pool_size >= min_pool);
  bool by_len  = (max_len  > 0) && (query_len <= max_len);
  if (logic_and) {
    /* Disabled arm => trivially satisfied. */
    if (min_pool == 0) by_pool = true;
    if (max_len  == 0) by_len  = true;
    return by_pool && by_len;
  }
  return by_pool || by_len;
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
  if (!env->eq(env, env->type_of(env, collection), Qvector)) {
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
  bool           fuzzy     = resolve_fzf_native_fuzzy(env);

  /* Decide filter-only mode from the two thresholds and the logic knob.
     Evaluated once before the workers spawn; the result rides on `shared'.
     Pool size for the decision is the candidate count we just batched. */
  size_t fo_min_pool = 0, fo_max_len = 0;
  bool   fo_logic_and = resolve_filter_only_settings(env, &fo_min_pool, &fo_max_len);
  size_t query_char_len = utf8_character_count(query.b, query.len);
  bool   filter_only_mode = decide_filter_only(fo_min_pool, fo_max_len,
                                               fo_logic_and,
                                               query_char_len, (size_t)n);
  fzf_log("fzf_native_score_all: filter_only=%d (min_pool=%zu max_len=%zu logic=%s qlen=%zu pool=%td)\n",
          (int)filter_only_mode, fo_min_pool, fo_max_len,
          fo_logic_and ? "and" : "or", query_char_len, n);

  fzf_pattern_t *pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
  struct Shared shared = {
    .pattern = pattern,
    .batches = batches,
    .remaining = batch_idx + 1,
    .filter_only = filter_only_mode,
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

  /* In full mode `xs[0]` becomes the highest-scoring candidate after the
     sort.  In filter-only mode we skip sorting — every survivor has
     score=1 from `fzf_has_match' so the sort would be a no-op — and
     preserve input order so callers (e.g. fussy) can run their own
     ranking against a stable, subsumable candidate set. */
  if (!filter_only_mode)
    counting_sort_candidates(xs, len);

  /* Resolve C-side highlight cap from fussy-fzf-native-highlight.  After
     the (possibly skipped) sort, xs[0..hl_cap-1] is the top-N to highlight
     — top-by-score in full mode, top-by-input-order in filter-only mode.
     The original parsing pattern was already freed; re-parse for
     highlighting using the same case mode the scoring used. */
  size_t hl_cap = resolve_fussy_highlight_cap(env, len);
  fzf_pattern_t *hl_pattern = NULL;
  fzf_slab_t    *hl_slab    = NULL;
  emacs_value    hl_hook    = Qnil;
  HlScratch      hl_scratch = { 0 };
  if (hl_cap > 0) {
    hl_pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
    if (hl_pattern) hl_slab = fzf_make_default_slab();
    if (!hl_slab) {
      if (hl_pattern) { fzf_free_pattern(hl_pattern); hl_pattern = NULL; }
      hl_cap = 0;
    }
    /* Read the highlight handler once per call; nil short-circuits
       per-candidate dispatch in `apply_highlight_positions'.  Pre-size
       the position scratch to the query length — the maximum possible
       number of matched positions per candidate. */
    if (hl_cap > 0) {
      hl_hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
      hl_scratch_init(&hl_scratch, query.len > 0 ? query.len : 1);
    }
  }

  for (size_t i = len; i-- > 0;) {
    /* Top-N candidates get a fresh copy before face/completion-score
       mutation so the caller's original strings (often long-lived shared
       objects: buffer names, obarray symbol-names, history lists) stay
       clean.  The tail (i >= hl_cap) never receives face here, so the
       only mutation it could see is `completion-score' — that's a
       single property at index 0, invisible to display, kept on the
       original to skip the copy cost. */
    bool highlight = hl_pattern && i < hl_cap;
    emacs_value out_val = highlight ? try_copy_string(env, xs[i].value) : xs[i].value;

    /* `completion-score' is a meaningful ranking signal in full mode but
       not in filter-only (every survivor scored 1 from fzf_has_match).
       Skip attaching it so downstream sort routines don't pick up a
       constant and treat it as a real ranking. */
    if (!filter_only_mode && xs[i].s.len > 0) {
      env->funcall(env, Fput_text_property, 5,
                   (emacs_value[]) {
                     Qzero, Qone, Qcompletion_score,
                     env->make_integer(env, xs[i].score), out_val });
    }

    if (highlight) {
      apply_highlight_positions(env, xs[i].s.b, hl_pattern, hl_slab,
                                out_val, hl_hook, &hl_scratch);
    }

    result = env->funcall(env, Fcons, 2, (emacs_value[]) { out_val, result });
  }

  if (hl_pattern) fzf_free_pattern(hl_pattern);
  if (hl_slab)    fzf_free_slab(hl_slab);
  hl_scratch_free(&hl_scratch);

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
    env->non_local_exit_signal(env, Qerror, Qnil);
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
// Returns COLLECTION with face-bearing copies substituted into the top-N
// slots (via aset for vectors, setcar for lists).  Caller's original
// candidate strings are not mutated on the apply path.
//
// The clear path unconditionally strips face from the top-N candidates,
// including the caller's originals when COLLECTION still holds them.  This
// is intentional and matches the function's contract: clear means "I'm
// telling you these strings carry stale face from a prior highlight pass;
// remove it."  Callers who want to preserve face on their originals across
// clears must hold their own pre-highlight copies.
emacs_value fzf_native_highlight_all(emacs_env *env,
                                     ptrdiff_t UNUSED(nargs),
                                     emacs_value args[],
                                     void UNUSED(*data_ptr)) {
  struct Bump *bump = NULL;
  fzf_pattern_t *pattern = NULL;
  fzf_slab_t    *slab    = NULL;
  /* Declared up here so the `goto done' early-exit paths below never skip
     the zero-initialization that `hl_scratch_free' relies on. */
  HlScratch hl_scratch = { 0 };
  emacs_value hook = Qnil;

  /* Treat an empty *or* undecodable query as clear-only.  The stale face
     properties live on the COLLECTION strings, not on the query, so we still
     need to walk the collection and strip face even if the query couldn't be
     coerced through `encode-coding-string'. */
  struct Str query = copy_emacs_string(env, &bump, args[1]);
  bool clear_only = (!query.b || query.len == 0);

  emacs_value len_v = env->funcall(env, Flength, 1, &args[0]);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    goto done;
  }
  ptrdiff_t n = (ptrdiff_t) env->extract_integer(env, len_v);
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
    bool           fuzzy     = resolve_fzf_native_fuzzy(env);
    pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
    if (!pattern) goto done;
    slab = fzf_make_default_slab();
    if (!slab) goto done;
    hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
    hl_scratch_init(&hl_scratch, query.len > 0 ? query.len : 1);
  }

  /* Explicit list / vector branches so each shape pays only its native
     traversal cost — vectors hit `vec_get'/`aset' as C-pointer calls (no
     Elisp boundary crossing); lists pay one `vconcat' (~equivalent to the
     legacy fast path) plus a single `cdr' per iteration to advance the
     cell cursor used for `setcar' substitution. */
  bool is_vector = env->eq(env, env->type_of(env, args[0]), Qvector);
  if (is_vector) {
    for (ptrdiff_t i = 0; i < (ptrdiff_t)hl_cap; i++) {
      emacs_value value = env->vec_get(env, args[0], i);
      if (clear_only) {
        clear_highlight_face(env, value);
      } else {
        struct Str s = copy_emacs_string(env, &bump, value);
        if (s.b) {
          emacs_value cp = try_copy_string(env, value);
          apply_highlight_positions(env, s.b, pattern, slab, cp, hook, &hl_scratch);
          env->funcall(env, Faset, 3,
                       (emacs_value[]) {
                         args[0], env->make_integer(env, i), cp });
          env->non_local_exit_clear(env);
        }
      }
    }
  } else {
    /* Lists: vconcat once for O(1) reads, then walk the original list in
       parallel one cdr per iter to keep a current-cell pointer for setcar
       substitution.  ~Half the funcalls of a pure car/cdr cursor. */
    emacs_value read_vec = env->funcall(env, Fvconcat, 1, &args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
      env->non_local_exit_clear(env);
      goto done;
    }
    emacs_value cell = args[0];
    for (ptrdiff_t i = 0; i < (ptrdiff_t)hl_cap; i++) {
      emacs_value value = env->vec_get(env, read_vec, i);
      if (clear_only) {
        clear_highlight_face(env, value);
      } else {
        struct Str s = copy_emacs_string(env, &bump, value);
        if (s.b) {
          emacs_value cp = try_copy_string(env, value);
          apply_highlight_positions(env, s.b, pattern, slab, cp, hook, &hl_scratch);
          env->funcall(env, Fsetcar, 2, (emacs_value[]) { cell, cp });
          env->non_local_exit_clear(env);
        }
      }
      cell = env->funcall(env, Fcdr, 1, &cell);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        break;
      }
    }
  }

done:
  if (slab)    fzf_free_slab(slab);
  if (pattern) fzf_free_pattern(pattern);
  hl_scratch_free(&hl_scratch);
  bump_free(bump);
  return args[0];
}

// fzf-native-highlight-one CAND QUERY -> propertized-copy
//
// Per-candidate counterpart to `fzf-native-highlight-all'.  Returns a
// fresh copy of CAND with `completions-common-part' face applied at
// the match positions of QUERY.  Intended for `completion-lazy-hilit-fn'
// callers (vertico / icomplete) that render highlights lazily per
// displayed candidate rather than eagerly across a whole top-N.
//
// Caller's original CAND is never mutated.  Empty/undecodable QUERY
// returns a fresh face-stripped copy (symmetric with the
// `highlight-all' clear-only branch).  No-match returns a fresh copy
// with no face applied.
//
// Unlike `highlight-all', this entry point ignores
// `fzf-native-batch-highlight' — the cap is meaningless for a single
// candidate.  The hook (`fzf-native-highlight-fn') is still honored;
// when nil, no face is applied.
emacs_value fzf_native_highlight_one(emacs_env *env,
                                     ptrdiff_t UNUSED(nargs),
                                     emacs_value args[],
                                     void UNUSED(*data_ptr)) {
  struct Bump *bump = NULL;
  fzf_pattern_t *pattern = NULL;
  fzf_slab_t    *slab    = NULL;
  HlScratch hl_scratch = { 0 };

  struct Str query = copy_emacs_string(env, &bump, args[1]);
  bool clear_only = (!query.b || query.len == 0);

  /* Fresh copy of CAND so caller's literal is never face-mutated.  Even
     the clear-only path acts on the copy, not the original. */
  emacs_value cp = try_copy_string(env, args[0]);

  if (clear_only) {
    clear_highlight_face(env, cp);
    goto done;
  }

  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  bool           fuzzy     = resolve_fzf_native_fuzzy(env);
  pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
  if (!pattern) goto done;
  slab = fzf_make_default_slab();
  if (!slab) goto done;
  emacs_value hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
  hl_scratch_init(&hl_scratch, query.len);

  struct Str s = copy_emacs_string(env, &bump, args[0]);
  if (s.b) {
    apply_highlight_positions(env, s.b, pattern, slab, cp, hook, &hl_scratch);
  }

done:
  if (slab)    fzf_free_slab(slab);
  if (pattern) fzf_free_pattern(pattern);
  hl_scratch_free(&hl_scratch);
  bump_free(bump);
  return cp;
}

/* Signal `(wrong-type-argument stringp VALUE)' if VALUE is not a string.
   Returns true on failure (caller should return immediately). */
static bool signal_if_not_string(emacs_env *env, emacs_value value) {
  if (env->eq(env, env->type_of(env, value), Qstring)) {
    return false;
  }
  emacs_value data_args[] = { Qstringp, value };
  env->non_local_exit_signal(env, Qwrong_type_argument,
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
  bool           fuzzy     = resolve_fzf_native_fuzzy(env);
  fzf_pattern_t *pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
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
    emacs_value hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
    apply_highlight_positions(env, str.b, pattern, slab, args[0], hook, NULL);
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
     larger completion batch. Empty QUERY short-circuits to the same
     value above; empty STR must reach the matcher because an inverse
     term can legitimately match it. */
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

#define ARENA_CHUNK_SIZE (4 * 1024 * 1024)  /* 4 MB per chunk */

/* Chunked candidate-pointer storage.
 *
 * The candidate-pointer table is split into fixed-size blocks owned by a
 * top-level pointer table.  The reader appends to the current block; when a
 * block fills, it allocates the next one.  No realloc ever moves pointer
 * data, so the worst-case allocation the reader performs is a single
 * block — predictable cost regardless of pool size.
 *
 *   cands_top[]        :  CANDS_TOP_CAP slots × 8 B  (~32 KB, fixed inline)
 *   cands_top[i]       :  CANDS_BLOCK_SIZE × 8 B     (2 MB, on demand)
 *
 * Defaults: 256K pointers per block, 4096 blocks → 1 G candidates max.
 *
 * Index split:  hi = i >> SHIFT  (which block)
 *               lo = i & MASK    (which slot in that block)
 * Both ops are single CPU instructions because BLOCK_SIZE is a power of 2.
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
  size_t          matched_count;
  fzf_case_types  case_mode;
  bool            fuzzy;
  bool            filter_only;
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

/* A raw byte-prefix proves subsumption only when Q' is one simple positive
   term.  Extending that term, or adding later AND terms, can only narrow its
   match set.  Operator-bearing sources need parsed-term proof: for example,
   extending !foo to !foobar broadens the inverse match, and treating it as a
   safe ancestor would silently discard valid candidates. */
static bool subsumes(const char *q_prime, const char *q) {
  if (strchr(q_prime, '|') || strchr(q, '|')) return false;
  size_t lp = strlen(q_prime);
  if (lp == 0) return true;
  if (strpbrk(q_prime, " \t\r\n!'^$\\")) return false;
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
                                            fzf_case_types case_mode,
                                            bool fuzzy) {
  if (!query || !*query) return NULL;
  char *dup = strdup(query);
  if (!dup) return NULL;
  fzf_pattern_t *p = fzf_parse_pattern(case_mode, false, dup, fuzzy);
  free(dup);
  return p;
}

/* Find an entry by exact matching semantics.  Filter-only mode is excluded:
   it changes ranking, but fzf_has_match preserves membership.  A lookup can
   therefore reuse m_idx across that mode boundary while rejecting top-K as
   authoritative.  Caller holds c->mu. */
static CacheEntry *cache_find_locked(Cache *c, const char *query,
                                     fzf_case_types case_mode, bool fuzzy) {
  for (CacheEntry *e = c->head; e; e = e->next)
    if (strcmp(e->query, query) == 0 &&
        e->case_mode == case_mode && e->fuzzy == fuzzy)
      return e;
  return NULL;
}

/* A cached top-K covers LIMIT when it contains every match, or when it holds
   at least the requested positive capacity.  LIMIT 0 requests every match. */
static bool cache_entry_covers_limit(const CacheEntry *e, size_t limit) {
  if (e->top_count >= e->matched_count) return true;
  return limit > 0 && e->top_count >= limit;
}

/* Exact lookup.  On hit, bumps entry to MRU and returns:
     *out_top, *out_top_count — caller-owned copy of the cached top-K
     *out_m_idx               — SharedIdx with refcount bumped (caller releases)
     *out_pool_gen            — pool size at the time this entry was scored
   Returns true on hit, false on miss. */
static bool cache_lookup_exact_for_request(
    Cache *c, const char *query,
    fzf_case_types case_mode, bool fuzzy,
    bool filter_only, size_t limit,
    ScoredStr **out_top, size_t *out_top_count,
    SharedIdx **out_m_idx, size_t *out_pool_gen,
    size_t *out_matched_count, bool *out_result_covered) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *e = cache_find_locked(c, query, case_mode, fuzzy);
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
  *out_matched_count = e->matched_count;
  bool top_copy_complete = e->top_count == 0 || top_copy != NULL;
  *out_result_covered = top_copy_complete &&
                        e->filter_only == filter_only &&
                        cache_entry_covers_limit(e, limit);

  /* Bump to MRU. */
  if (e != c->head) { cache_unlink_locked(c, e); cache_push_head_locked(c, e); }
  pthread_mutex_unlock(&c->mu);
  return true;
}

/* Return only the membership evidence for an exact semantic key.  Automatic
   input-growth retries do not need a top-K copy because the last completed
   public result stays visible until the retry finishes. */
static bool cache_lookup_membership_exact(
    Cache *c, const char *query, fzf_case_types case_mode, bool fuzzy,
    SharedIdx **out_m_idx, size_t *out_pool_gen) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *e = cache_find_locked(c, query, case_mode, fuzzy);
  if (!e) {
    pthread_mutex_unlock(&c->mu);
    return false;
  }
  *out_m_idx = shared_idx_retain(e->m_idx);
  *out_pool_gen = e->pool_gen;
  if (e != c->head) {
    cache_unlink_locked(c, e);
    cache_push_head_locked(c, e);
  }
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
                                fzf_case_types case_mode, bool fuzzy,
                                ScoredStr **out_top, size_t *out_top_count,
                                SharedIdx **out_m_idx, size_t *out_pool_gen) {
  if (strchr(query, '|')) return false;   /* fast reject */

  fzf_pattern_t *p_query = parse_query_for_cache(query, case_mode, fuzzy);
  /* If parse failed and query isn't empty, fall back to byte-prefix only.
     Empty query has p_query == NULL but byte-prefix subsumes("", anything)
     also returns true so the loop still works. */

  pthread_mutex_lock(&c->mu);
  CacheEntry *best = NULL;
  size_t best_terms = 0;
  size_t best_len   = 0;
  for (CacheEntry *e = c->head; e; e = e->next) {
    if (e->case_mode != case_mode || e->fuzzy != fuzzy) continue;
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
static void cache_insert_for_request(
    Cache *c, const char *query, size_t pool_gen,
    fzf_case_types case_mode, bool fuzzy, bool filter_only,
    const ScoredStr *top, size_t top_count, size_t matched_count,
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
  fzf_pattern_t *parsed = parse_query_for_cache(query, case_mode, fuzzy);

  if (!q_dup) {
    free(top_dup);
    shared_idx_release(sidx);
    if (parsed) fzf_free_pattern(parsed);
    return;
  }

  pthread_mutex_lock(&c->mu);
  CacheEntry *e = cache_find_locked(c, query, case_mode, fuzzy);
  if (e) {
    /* Update existing entry: swap fields, release old refs after unlock. */
    char *old_q = e->query;
    ScoredStr *old_top = e->top;
    SharedIdx *old_idx = e->m_idx;
    fzf_pattern_t *old_parsed = e->parsed;
    e->query     = q_dup;
    e->top       = top_dup;
    e->top_count = top_dup ? top_count : 0;
    e->matched_count = matched_count;
    e->case_mode = case_mode;
    e->fuzzy = fuzzy;
    e->filter_only = filter_only;
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
  ne->matched_count = matched_count;
  ne->case_mode = case_mode;
  ne->fuzzy = fuzzy;
  ne->filter_only = filter_only;
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

#ifdef FZF_NATIVE_CTEST
/* Keep the older compact signatures for tests that exercise generic cache
   mechanics.  Semantic and capacity tests call the request-aware functions
   directly. */
static bool cache_lookup_exact(Cache *c, const char *query,
                               ScoredStr **out_top, size_t *out_top_count,
                               SharedIdx **out_m_idx, size_t *out_pool_gen) {
  size_t matched_count = 0;
  bool covered = false;
  return cache_lookup_exact_for_request(
      c, query, CaseSmart, true, false, 0,
      out_top, out_top_count, out_m_idx, out_pool_gen,
      &matched_count, &covered);
}

static void cache_insert(Cache *c, const char *query, size_t pool_gen,
                         fzf_case_types case_mode, bool fuzzy,
                         const ScoredStr *top, size_t top_count,
                         const uint32_t *m_idx_src, size_t m_idx_count) {
  size_t matched_count = m_idx_count ? m_idx_count : top_count;
  cache_insert_for_request(c, query, pool_gen, case_mode, fuzzy, false,
                           top, top_count, matched_count,
                           m_idx_src, m_idx_count);
}
#endif

/* Stable full-batch membership cache.  A completed worker stores the match
   set for its immutable BATCH_SIZE candidate range before the coordinator
   checks whether the request was superseded.  A later exact or narrowing
   query can then score only candidates that the cached broader query matched.

   Query records own the parsed pattern once per semantic query.  Entries use
   either uint16_t local indexes for sparse sets or a fixed bitmap for denser
   sets.  Sets above 50 percent selectivity are not cached. */
typedef struct BatchQuery BatchQuery;
typedef struct BatchCacheEntry BatchCacheEntry;

struct BatchQuery {
  BatchQuery     *next;
  char           *query;
  fzf_pattern_t  *parsed;
  fzf_case_types  case_mode;
  bool            fuzzy;
  size_t          entry_count;
  size_t          external_refs;
  size_t          bytes;
};

enum BatchMembershipKind {
  BatchMembershipSparse,
  BatchMembershipBitmap,
};

struct BatchCacheEntry {
  BatchCacheEntry *hash_next;
  BatchCacheEntry *lru_prev;
  BatchCacheEntry *lru_next;
  BatchQuery      *owner;
  size_t           batch_id;
  size_t           match_count;
  size_t           bytes;
  enum BatchMembershipKind kind;
  union {
    uint16_t *sparse;
    uint64_t *bitmap;
  } members;
};

typedef struct {
  pthread_mutex_t  mu;
  BatchQuery      *queries;
  BatchCacheEntry *head;
  BatchCacheEntry *tail;
  BatchCacheEntry **buckets;
  size_t           bucket_count;
  size_t           used_bytes;
  size_t           max_bytes;
  uint64_t         hits;
  uint64_t         misses;
  uint64_t         inserts;
  uint64_t         evictions;
} BatchCache;

static size_t batch_cache_hash(const BatchCache *c, const BatchQuery *query,
                               size_t batch_id) {
  uintptr_t q = (uintptr_t)query;
  uint64_t x = (uint64_t)(q >> 4) ^
               ((uint64_t)batch_id * UINT64_C(11400714819323198485));
  return c->bucket_count ? (size_t)(x % c->bucket_count) : 0;
}

static void batch_cache_init(BatchCache *c, size_t max_bytes) {
  memset(c, 0, sizeof *c);
  pthread_mutex_init(&c->mu, NULL);
  c->max_bytes = max_bytes;
  c->bucket_count = BATCH_CACHE_BUCKETS;
  c->buckets = calloc(c->bucket_count, sizeof *c->buckets);
  if (!c->buckets) {
    c->bucket_count = 0;
    c->max_bytes = 0;
  }
}

static BatchQuery *batch_cache_find_query_locked(
    BatchCache *c, const char *query,
    fzf_case_types case_mode, bool fuzzy) {
  for (BatchQuery *q = c->queries; q; q = q->next)
    if (q->case_mode == case_mode && q->fuzzy == fuzzy &&
        strcmp(q->query, query) == 0)
      return q;
  return NULL;
}

static void batch_cache_remove_query_locked(BatchCache *c, BatchQuery *query) {
  BatchQuery **slot = &c->queries;
  while (*slot && *slot != query) slot = &(*slot)->next;
  if (*slot == query) *slot = query->next;
  if (c->used_bytes >= query->bytes) c->used_bytes -= query->bytes;
  free(query->query);
  if (query->parsed) fzf_free_pattern(query->parsed);
  free(query);
}

static BatchQuery *batch_cache_acquire_query(
    BatchCache *c, const char *query,
    fzf_case_types case_mode, bool fuzzy) {
  if (!c->max_bytes || !c->buckets) return NULL;

  char *query_copy = strdup(query);
  fzf_pattern_t *parsed = parse_query_for_cache(query, case_mode, fuzzy);
  BatchQuery *created = calloc(1, sizeof *created);
  if (!query_copy || !created) {
    free(query_copy);
    if (parsed) fzf_free_pattern(parsed);
    free(created);
    return NULL;
  }
  created->query = query_copy;
  created->parsed = parsed;
  created->case_mode = case_mode;
  created->fuzzy = fuzzy;
  created->bytes = sizeof *created + strlen(query_copy) + 1;

  pthread_mutex_lock(&c->mu);
  BatchQuery *found = batch_cache_find_query_locked(
      c, query, case_mode, fuzzy);
  if (found) {
    found->external_refs++;
    pthread_mutex_unlock(&c->mu);
    free(created->query);
    if (created->parsed) fzf_free_pattern(created->parsed);
    free(created);
    return found;
  }
  created->external_refs = 1;
  created->next = c->queries;
  c->queries = created;
  c->used_bytes += created->bytes;
  pthread_mutex_unlock(&c->mu);
  return created;
}

static void batch_cache_release_query(BatchCache *c, BatchQuery *query) {
  if (!query) return;
  pthread_mutex_lock(&c->mu);
  if (query->external_refs > 0) query->external_refs--;
  if (query->external_refs == 0 && query->entry_count == 0)
    batch_cache_remove_query_locked(c, query);
  pthread_mutex_unlock(&c->mu);
}

/* Select the most constrained cached query whose match set is a superset of
   TARGET.  Exact membership is preferred.  OR queries participate only in
   exact reuse because adding an OR alternate is not monotonic. */
static BatchQuery *batch_cache_select_source(
    BatchCache *c, const char *target,
    fzf_case_types case_mode, bool fuzzy) {
  if (!c->max_bytes || !c->buckets) return NULL;
  fzf_pattern_t *target_pattern = parse_query_for_cache(
      target, case_mode, fuzzy);
  BatchQuery *best = NULL;
  size_t best_terms = 0;
  size_t best_len = 0;

  pthread_mutex_lock(&c->mu);
  for (BatchQuery *q = c->queries; q; q = q->next) {
    if (q->entry_count == 0 || q->case_mode != case_mode ||
        q->fuzzy != fuzzy)
      continue;
    if (strcmp(q->query, target) == 0) {
      best = q;
      break;
    }
    bool safe = subsumes(q->query, target) ||
                (target_pattern &&
                 subsumes_pattern(q->parsed, target_pattern));
    if (!safe) continue;
    size_t terms = q->parsed ? q->parsed->size : 0;
    size_t len = strlen(q->query);
    if (!best || terms > best_terms ||
        (terms == best_terms && len > best_len)) {
      best = q;
      best_terms = terms;
      best_len = len;
    }
  }
  if (best) best->external_refs++;
  pthread_mutex_unlock(&c->mu);
  if (target_pattern) fzf_free_pattern(target_pattern);
  return best;
}

static BatchCacheEntry *batch_cache_find_entry_locked(
    BatchCache *c, BatchQuery *query, size_t batch_id) {
  if (!c->buckets || c->bucket_count == 0) return NULL;
  size_t bucket = batch_cache_hash(c, query, batch_id);
  for (BatchCacheEntry *e = c->buckets[bucket]; e; e = e->hash_next)
    if (e->owner == query && e->batch_id == batch_id)
      return e;
  return NULL;
}

static void batch_cache_lru_unlink_locked(BatchCache *c,
                                          BatchCacheEntry *entry) {
  if (entry->lru_prev) entry->lru_prev->lru_next = entry->lru_next;
  else c->head = entry->lru_next;
  if (entry->lru_next) entry->lru_next->lru_prev = entry->lru_prev;
  else c->tail = entry->lru_prev;
  entry->lru_prev = entry->lru_next = NULL;
}

static void batch_cache_lru_push_locked(BatchCache *c,
                                        BatchCacheEntry *entry) {
  entry->lru_prev = NULL;
  entry->lru_next = c->head;
  if (c->head) c->head->lru_prev = entry;
  else c->tail = entry;
  c->head = entry;
}

static void batch_cache_entry_free(BatchCacheEntry *entry) {
  if (!entry) return;
  if (entry->kind == BatchMembershipSparse) free(entry->members.sparse);
  else free(entry->members.bitmap);
  free(entry);
}

static void batch_cache_remove_entry_locked(BatchCache *c,
                                            BatchCacheEntry *entry,
                                            bool eviction) {
  size_t bucket = batch_cache_hash(c, entry->owner, entry->batch_id);
  BatchCacheEntry **slot = &c->buckets[bucket];
  while (*slot && *slot != entry) slot = &(*slot)->hash_next;
  if (*slot == entry) *slot = entry->hash_next;
  batch_cache_lru_unlink_locked(c, entry);
  if (c->used_bytes >= entry->bytes) c->used_bytes -= entry->bytes;
  BatchQuery *owner = entry->owner;
  if (owner->entry_count > 0) owner->entry_count--;
  if (eviction) c->evictions++;
  batch_cache_entry_free(entry);
  if (owner->entry_count == 0 && owner->external_refs == 0)
    batch_cache_remove_query_locked(c, owner);
}

/* Copy one cached membership set as local indexes in [0, BATCH_SIZE). */
static bool batch_cache_copy_members(BatchCache *c, BatchQuery *query,
                                     size_t batch_id, uint16_t *out,
                                     size_t *out_count) {
  if (!query || !out_count) return false;
  pthread_mutex_lock(&c->mu);
  BatchCacheEntry *entry = batch_cache_find_entry_locked(
      c, query, batch_id);
  if (!entry) {
    c->misses++;
    pthread_mutex_unlock(&c->mu);
    return false;
  }
  size_t n = 0;
  if (entry->kind == BatchMembershipSparse) {
    n = entry->match_count;
    if (n && out) memcpy(out, entry->members.sparse, n * sizeof *out);
  } else {
    for (size_t word = 0; word < BATCH_SIZE / 64; word++) {
      uint64_t bits = entry->members.bitmap[word];
      while (bits) {
        unsigned bit = (unsigned)__builtin_ctzll(bits);
        if (out) out[n] = (uint16_t)(word * 64 + bit);
        n++;
        bits &= bits - 1;
      }
    }
  }
  *out_count = n;
  c->hits++;
  if (entry != c->head) {
    batch_cache_lru_unlink_locked(c, entry);
    batch_cache_lru_push_locked(c, entry);
  }
  pthread_mutex_unlock(&c->mu);
  return true;
}

static void batch_cache_insert(BatchCache *c, BatchQuery *query,
                               size_t batch_id, const ScoredStr *matches,
                               size_t match_count) {
  if (!query || !c->max_bytes || !c->buckets ||
      match_count > BATCH_SIZE / 2)
    return;

  BatchCacheEntry *created = calloc(1, sizeof *created);
  if (!created) return;
  created->owner = query;
  created->batch_id = batch_id;
  created->match_count = match_count;
  created->bytes = sizeof *created;
  if (match_count <= BATCH_CACHE_SPARSE_LIMIT) {
    created->kind = BatchMembershipSparse;
    if (match_count) {
      created->members.sparse = malloc(
          match_count * sizeof *created->members.sparse);
      if (!created->members.sparse) {
        free(created);
        return;
      }
      for (size_t i = 0; i < match_count; i++)
        created->members.sparse[i] =
            (uint16_t)(matches[i].idx % BATCH_SIZE);
      created->bytes += match_count * sizeof *created->members.sparse;
    }
  } else {
    created->kind = BatchMembershipBitmap;
    size_t words = BATCH_SIZE / 64;
    created->members.bitmap = calloc(words, sizeof *created->members.bitmap);
    if (!created->members.bitmap) {
      free(created);
      return;
    }
    for (size_t i = 0; i < match_count; i++) {
      size_t local = matches[i].idx % BATCH_SIZE;
      created->members.bitmap[local / 64] |= UINT64_C(1) << (local % 64);
    }
    created->bytes += words * sizeof *created->members.bitmap;
  }
  if (created->bytes > c->max_bytes) {
    batch_cache_entry_free(created);
    return;
  }

  pthread_mutex_lock(&c->mu);
  BatchCacheEntry *old = batch_cache_find_entry_locked(
      c, query, batch_id);
  if (old) batch_cache_remove_entry_locked(c, old, false);

  size_t bucket = batch_cache_hash(c, query, batch_id);
  created->hash_next = c->buckets[bucket];
  c->buckets[bucket] = created;
  batch_cache_lru_push_locked(c, created);
  query->entry_count++;
  c->used_bytes += created->bytes;
  c->inserts++;

  while (c->tail && c->used_bytes > c->max_bytes)
    batch_cache_remove_entry_locked(c, c->tail, true);
  pthread_mutex_unlock(&c->mu);
}

static void batch_cache_stats(BatchCache *c, size_t *entries,
                              size_t *bytes, uint64_t *hits,
                              uint64_t *misses, uint64_t *evictions) {
  pthread_mutex_lock(&c->mu);
  size_t count = 0;
  for (BatchCacheEntry *e = c->head; e; e = e->lru_next) count++;
  *entries = count;
  *bytes = c->used_bytes;
  *hits = c->hits;
  *misses = c->misses;
  *evictions = c->evictions;
  pthread_mutex_unlock(&c->mu);
}

static void batch_cache_free(BatchCache *c) {
  pthread_mutex_lock(&c->mu);
  BatchCacheEntry *entry = c->head;
  while (entry) {
    BatchCacheEntry *next = entry->lru_next;
    batch_cache_entry_free(entry);
    entry = next;
  }
  BatchQuery *query = c->queries;
  while (query) {
    BatchQuery *next = query->next;
    free(query->query);
    if (query->parsed) fzf_free_pattern(query->parsed);
    free(query);
    query = next;
  }
  free(c->buckets);
  c->buckets = NULL;
  c->queries = NULL;
  c->head = c->tail = NULL;
  pthread_mutex_unlock(&c->mu);
  pthread_mutex_destroy(&c->mu);
}

struct AsyncWorkerPool;
static struct AsyncWorkerPool *async_worker_pool_create(unsigned count);
static void async_worker_pool_destroy(struct AsyncWorkerPool *pool);

enum AsyncChildOwner {
  AsyncChildUnclaimed,
  AsyncChildReader,
  AsyncChildTeardown,
};

enum AsyncProducerState {
  AsyncProducerRunning,
  AsyncProducerComplete,
  AsyncProducerFailed,
  AsyncProducerStopped,
};

enum AsyncProducerErrorKind {
  AsyncProducerErrorNone,
  AsyncProducerErrorExit,
  AsyncProducerErrorSignal,
  AsyncProducerErrorRead,
  AsyncProducerErrorAllocation,
  AsyncProducerErrorCapacity,
  AsyncProducerErrorWait,
};

typedef struct {
  pthread_t     reader;
  bool          reader_started;
  pid_t         pid;
  _Atomic int   child_owner;
  _Atomic int   producer_state;
  _Atomic int   producer_error_kind;
  _Atomic int   producer_error_number;
  _Atomic int   producer_exit_status;
  FILE         *fp;
  _Atomic bool stop;
  /* Set by the reader thread immediately before it exits — either
     because the child producer closed its stdout (EOF) or because
     `stop' was raised during teardown.  Read by
     `fzf-native-async-result-fresh-p' to distinguish "pool=0 because
     the producer hasn't streamed yet" (not authoritative) from
     "pool=0 because the producer finished without emitting anything"
     (authoritative zero). */
  _Atomic bool reader_done;

  pthread_mutex_t mu;
  Arena           arena;   /* backing storage for all candidate strings */
  /* Two-level pointer table; see CANDS_BLOCK_SHIFT comments above.
     Top level is fixed-size and zero-initialized at session start;
     blocks are allocated on demand by the reader.  Access pattern:
     cands_top[i >> CANDS_BLOCK_SHIFT][i & CANDS_BLOCK_MASK]. */
  char          **cands_top[CANDS_TOP_CAP];
  size_t          count;
  _Atomic int     gen;

  size_t          last_filtered;   /* candidates matching last filter */
  size_t          last_total;      /* total candidates at last call */

  /* Background scoring thread */
  pthread_t        score_thread;
  bool             score_thread_started;
  struct AsyncWorkerPool *worker_pool;
  pthread_mutex_t  score_req_mu;
  pthread_cond_t   score_req_cond;
  uint64_t         score_next_id;
  uint64_t         score_latest_id;
  char            *score_latest_filter;
  size_t           score_latest_limit;
  fzf_case_types   score_latest_case_mode;
  bool             score_latest_fuzzy;
  size_t           score_latest_filter_only_length;
  bool             score_latest_filter_only_logic_and;
  _Atomic bool     score_has_request;
  /* The reader changes false -> true once for each coalesced growth epoch.
     The scorer clears it when one attempt takes ownership of the newest
     candidate boundary. */
  _Atomic bool     score_growth_pending;
  uint64_t         score_req_id;
  char            *score_req_filter;  /* owned; NULL = nothing pending */
  size_t           score_req_limit;
  fzf_case_types   score_req_case_mode;
  bool             score_req_fuzzy;
  /* Refinement request: when score_req_refine_idx is non-NULL the next scoring
     run scores only those candidate indices plus s->cands[refine_delta_from..count].
     Ownership transfers to the scoring thread along with score_req_filter. */
  SharedIdx       *score_req_refine_idx;
  size_t           score_req_refine_delta_from;
  /* Filter-only request settings, snapshot of the user-facing defcustoms
     at dispatch time (main thread has the emacs_env *; scoring thread
     does not).  `score_req_filter_only_length` 0 == disabled arm;
     `score_req_filter_only_logic_and` selects AND vs OR composition with
     `s->filter_only_min_pool`. */
  size_t           score_req_filter_only_length;
  bool             score_req_filter_only_logic_and;
  bool             score_req_stop;
  _Atomic bool     score_abort;       /* set to cancel in-flight workers */

  uint64_t         score_current_id;
  char            *score_current_filter; /* filter being actively scored (under score_req_mu) */
  size_t           score_current_limit;
  fzf_case_types   score_current_case_mode;
  bool             score_current_fuzzy;
  size_t           score_current_filter_only_length;
  bool             score_current_filter_only_logic_and;
  _Atomic size_t   score_progress_completed;
  _Atomic size_t   score_progress_total;

  pthread_mutex_t  score_res_mu;
  ScoredStr       *score_results;     /* latest scored+sorted results */
  size_t           score_count;       /* number of entries in score_results */
  uint64_t         score_result_id;
  char            *score_result_filter;
  size_t           score_result_limit;
  fzf_case_types   score_result_case_mode;
  bool             score_result_fuzzy;
  bool             score_result_filter_only;
  size_t           score_result_pool_gen;
  uint64_t         score_snapshot_generation;
  size_t           score_result_progress_completed;
  size_t           score_result_progress_total;
  uint64_t         score_error_id;
  char            *score_error;

  /* Result cache (LRU keyed by query, values include matched_idx for
     prefix refinement).  Read on dispatch (main thread); written on
     scoring publish (scoring thread). */
  Cache            cache;
  BatchCache       batch_cache;

  /* Read-only after session start; set from fzf-async-max-line-length defcustom.
     0 = no limit.  >0 = exclude lines longer than N chars.  <0 = truncate to |N|. */
  ptrdiff_t        max_line_length;

  /* Filter-only mode threshold.  When > 0 and the pool reaches this
     size, the scoring thread skips full fzf evaluation in favor of
     `fzf_has_match' (see fzf-additions.c) and emits results in pool
     order, capped at the limit.  Match-set is still exhaustive so
     prefix-refinement remains correct across the threshold crossing.
     See `fzf-native-filter-only-min-pool'. */
  size_t           filter_only_min_pool;
} AsyncSession;

static int async_normalize_wait_status(int status) {
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
  return -1;
}

static void async_record_producer_failure(AsyncSession *s,
                                          enum AsyncProducerErrorKind kind,
                                          int error_number) {
  int expected = AsyncProducerErrorNone;
  if (atomic_compare_exchange_strong_explicit(
          &s->producer_error_kind, &expected, kind,
          memory_order_acq_rel, memory_order_acquire))
    atomic_store_explicit(&s->producer_error_number, error_number,
                          memory_order_release);
}

static bool async_claim_child(AsyncSession *s, enum AsyncChildOwner owner) {
  int expected = AsyncChildUnclaimed;
  return atomic_compare_exchange_strong_explicit(
      &s->child_owner, &expected, owner,
      memory_order_acq_rel, memory_order_acquire);
}

static void async_publish_score_failure(AsyncSession *s, uint64_t request_id,
                                        const char *message) {
  char *copy = message ? strdup(message) : NULL;
  pthread_mutex_lock(&s->score_req_mu);
  if (s->score_current_id == request_id) {
    s->score_current_id = 0;
    free(s->score_current_filter);
    s->score_current_filter = NULL;
  }
  pthread_mutex_lock(&s->score_res_mu);
  if (request_id >= s->score_error_id) {
    free(s->score_error);
    s->score_error = copy;
    copy = NULL;
    s->score_error_id = request_id;
    s->score_snapshot_generation++;
  }
  pthread_mutex_unlock(&s->score_res_mu);
  pthread_mutex_unlock(&s->score_req_mu);
  free(copy);
  atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
}

static void async_notify_candidate_growth(AsyncSession *s) {
  if (!atomic_load_explicit(&s->score_has_request, memory_order_acquire))
    return;
  bool was_pending = atomic_exchange_explicit(
      &s->score_growth_pending, true, memory_order_acq_rel);
  if (was_pending) return;
  pthread_mutex_lock(&s->score_req_mu);
  pthread_cond_signal(&s->score_req_cond);
  pthread_mutex_unlock(&s->score_req_mu);
}

/* Append one immutable candidate to the session arena and publish it to the
   scorer.  The producer is the sole caller, so reading COUNT and the current
   top-level block pointer before taking MU is safe.  Keeping this operation
   separate from getline lets the native interactive fuzzer drive the real
   growth path without an Emacs process or a shell child. */
static bool async_append_candidate(AsyncSession *s, const char *line,
                                   size_t len) {
  size_t i  = s->count;
  size_t hi = i >> CANDS_BLOCK_SHIFT;
  size_t lo = i & CANDS_BLOCK_MASK;
  if (hi >= CANDS_TOP_CAP) {
    size_t preview = len > 80 ? 80 : len;
    (void)preview;
    fzf_log("async_reader: TOP TABLE FULL count=%zu cap=%zu line='%.*s%s'\n",
            s->count, (size_t)CANDS_TOP_CAP * CANDS_BLOCK_SIZE,
            (int)preview, line, len > preview ? "..." : "");
    async_record_producer_failure(s, AsyncProducerErrorCapacity, 0);
    return false;
  }

  char *dup = arena_strdup(&s->arena, line, len);
  if (!dup) {
    async_record_producer_failure(
        s, AsyncProducerErrorAllocation, ENOMEM);
    return false;
  }

  /* Pre-allocate the new block outside MU.  This is the largest allocation
     in the append path, and publishing the pointer under MU makes the block
     visible atomically with the first candidate stored in it. */
  char **block = s->cands_top[hi];
  bool need_publish = block == NULL;
  if (need_publish) {
    block = malloc(CANDS_BLOCK_SIZE * sizeof *block);
    if (!block) {
      async_record_producer_failure(
          s, AsyncProducerErrorAllocation, ENOMEM);
      return false;
    }
    fzf_log("async_reader: allocated block %zu (count=%zu, %zu MB)\n",
            hi, s->count, (CANDS_BLOCK_SIZE * sizeof *block) >> 20);
  }

  pthread_mutex_lock(&s->mu);
  if (need_publish) s->cands_top[hi] = block;
  s->cands_top[hi][lo] = dup;
  s->count++;
  pthread_mutex_unlock(&s->mu);
  atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
  async_notify_candidate_growth(s);
  return true;
}

static void *async_reader(void *arg) {
  fzf_block_all_signals();
  AsyncSession *s = arg;
  fzf_log("async_reader START: pid=%d\n", (int)s->pid);
  /* getline manages a growable buffer that delivers whole logical
     lines regardless of length.  Pre-getline, the reader used fgets
     with a fixed 8 KB stack buffer and fragmented long lines at
     arbitrary I/O boundaries.

     No internal hard ceiling — matches the semantics of fzf, ripgrep,
     and GNU grep, which all let the reader buffer grow until either
     the line ends or the OS denies allocation.  The user-facing knob
     `fzf-native-max-line-length' is checked *after* getline returns
     (analogous to ripgrep's `--max-columns'), so pathological lines
     are filtered from the candidate stream but the read itself still
     happens.  Users who need to truncate before reading can set the
     producer's output up-front (e.g. `awk 'length<256'`). */
  char  *line   = NULL;
  size_t bufcap = 0;
  ssize_t glen;
  while (!atomic_load_explicit(&s->stop, memory_order_relaxed) && s->fp &&
         (glen = getline(&line, &bufcap, s->fp)) != -1) {
    size_t len = (size_t)glen;
    while (len && (line[len - 1] == '\n' || line[len - 1] == '\r'))
      line[--len] = '\0';
    len = async_strip_ansi(line, len);
    if (!len) continue;

    ptrdiff_t mll = s->max_line_length;
    if (mll != 0) {
      size_t cap = mll > 0
                     ? (size_t)mll
                     : (size_t)(-(mll + 1)) + 1;
      size_t char_len = utf8_character_count(line, len);
      if (char_len > cap) {
        if (mll > 0) continue;   /* exclude */
        len = utf8_prefix_byte_length(line, len, cap); /* truncate */
        line[len] = '\0';
      }
    }

    if (!async_append_candidate(s, line, len)) break;
  }
  bool stopping = atomic_load_explicit(&s->stop, memory_order_relaxed);
  if (!stopping && s->fp && !feof(s->fp) &&
      atomic_load_explicit(&s->producer_error_kind,
                           memory_order_acquire) == AsyncProducerErrorNone)
    async_record_producer_failure(
        s, AsyncProducerErrorRead, errno ? errno : EIO);

  free(line);
  if (async_claim_child(s, AsyncChildReader)) {
    enum AsyncProducerErrorKind error_kind =
        (enum AsyncProducerErrorKind)atomic_load_explicit(
            &s->producer_error_kind, memory_order_acquire);
    if (!stopping && error_kind != AsyncProducerErrorNone)
      kill(s->pid, SIGKILL);

    int wait_status = 0;
    pid_t waited;
    do {
      waited = waitpid(s->pid, &wait_status, 0);
    } while (waited < 0 && errno == EINTR);

    int exit_status = waited == s->pid
                          ? async_normalize_wait_status(wait_status)
                          : -1;
    atomic_store_explicit(&s->producer_exit_status, exit_status,
                          memory_order_release);
    if (stopping) {
      atomic_store_explicit(&s->producer_state, AsyncProducerStopped,
                            memory_order_release);
    } else if (error_kind != AsyncProducerErrorNone) {
      atomic_store_explicit(&s->producer_state, AsyncProducerFailed,
                            memory_order_release);
    } else if (waited != s->pid) {
      async_record_producer_failure(
          s, AsyncProducerErrorWait, errno ? errno : ECHILD);
      atomic_store_explicit(&s->producer_state, AsyncProducerFailed,
                            memory_order_release);
    } else if (WIFEXITED(wait_status) && WEXITSTATUS(wait_status) == 0) {
      atomic_store_explicit(&s->producer_state, AsyncProducerComplete,
                            memory_order_release);
    } else {
      async_record_producer_failure(
          s, WIFSIGNALED(wait_status) ? AsyncProducerErrorSignal
                                     : AsyncProducerErrorExit,
          WIFSIGNALED(wait_status) ? WTERMSIG(wait_status) : exit_status);
      atomic_store_explicit(&s->producer_state, AsyncProducerFailed,
                            memory_order_release);
    }
  }
  atomic_store_explicit(&s->reader_done, true, memory_order_release);
  fzf_log("async_reader EXIT: total=%zu gen=%d\n",
          s->count, (int)atomic_load_explicit(&s->gen, memory_order_relaxed));
  return NULL;
}

static void *scoring_thread_fn(void *arg);  /* defined after worker-pool helpers */

/* Test-visible counter: incremented once per detached destroy completion.
   The ctest binary polls this to confirm a deferred teardown actually
   ran; production builds increment-only and never read it. */
static _Atomic uint64_t async_destroy_completions;

static void async_session_destroy(void *ptr) {
  AsyncSession *s = ptr;
  if (!s) return;
  fzf_log("async_session_destroy: pid=%d count=%zu\n", (int)s->pid, s->count);

  /* Signal everything to stop simultaneously so scoring and reader wind down
     in parallel rather than sequentially. */
  atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);
  atomic_store_explicit(&s->stop, true, memory_order_relaxed);
  if (s->pid > 0 && async_claim_child(s, AsyncChildTeardown))
    kill(s->pid, SIGTERM);   /* reader unblocks on pipe EOF */
  atomic_store_explicit(&s->producer_state, AsyncProducerStopped,
                        memory_order_release);

  pthread_mutex_lock(&s->score_req_mu);
  free(s->score_req_filter);
  s->score_req_filter = NULL;
  shared_idx_release(s->score_req_refine_idx);
  s->score_req_refine_idx = NULL;
  s->score_req_stop   = true;
  pthread_cond_signal(&s->score_req_cond);
  pthread_mutex_unlock(&s->score_req_mu);
  if (s->score_thread_started)
    pthread_join(s->score_thread, NULL);

  async_worker_pool_destroy(s->worker_pool);
  s->worker_pool = NULL;

  /* The reader signals score_req_cond when it appends a candidate.  Join it
     before the request mutex and condition variable are destroyed. */
  if (s->reader_started)
    pthread_join(s->reader, NULL);

  free(s->score_results);
  free(s->score_result_filter);
  free(s->score_error);
  free(s->score_current_filter);
  free(s->score_latest_filter);
  cache_free(&s->cache);
  batch_cache_free(&s->batch_cache);
  pthread_mutex_destroy(&s->score_res_mu);
  pthread_mutex_destroy(&s->score_req_mu);
  pthread_cond_destroy(&s->score_req_cond);

  if (s->fp)      { fclose(s->fp); s->fp = NULL; }
  if (s->pid > 0 &&
      atomic_load_explicit(&s->child_owner,
                           memory_order_acquire) == AsyncChildTeardown) {
    int status = 0;
    while (waitpid(s->pid, &status, 0) < 0 && errno == EINTR) {}
  }
  pthread_mutex_lock(&s->mu);
  arena_free(&s->arena);
  for (size_t k = 0; k < CANDS_TOP_CAP; k++)
    if (s->cands_top[k]) { free(s->cands_top[k]); s->cands_top[k] = NULL; }
  pthread_mutex_unlock(&s->mu);
  pthread_mutex_destroy(&s->mu);
  free(s);
  atomic_fetch_add_explicit(&async_destroy_completions, 1,
                            memory_order_relaxed);
}

static void *async_destroy_worker(void *arg) {
  fzf_block_all_signals();
  async_session_destroy(arg);
  return NULL;
}

/* Non-blocking teardown.  Performs the cheap signaling on the caller
   thread so the subprocess and worker threads start winding down
   immediately, then offloads the blocking pthread_join + arena_free +
   cache_free to a detached pthread.  Returns within microseconds even
   for sessions with tens of millions of candidates.

   `kill(pid, SIGKILL)` (vs SIGTERM in async_session_destroy) makes the
   subprocess die immediately rather than running its own shutdown path.
   The reader thread sees pipe EOF and exits; the scoring thread
   short-circuits on score_abort.  The detached worker then joins both
   without holding up the Emacs main thread on minibuffer dismissal.

   Falls back to a synchronous destroy if pthread_create fails — the
   join cost is preferable to a leak. */
static void async_session_destroy_async(void *ptr) {
  AsyncSession *s = ptr;
  if (!s) return;
  fzf_log("async_session_destroy_async: pid=%d count=%zu\n",
          (int)s->pid, s->count);

  atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);
  atomic_store_explicit(&s->stop, true, memory_order_relaxed);
  if (s->pid > 0 && async_claim_child(s, AsyncChildTeardown))
    kill(s->pid, SIGKILL);
  atomic_store_explicit(&s->producer_state, AsyncProducerStopped,
                        memory_order_release);

  pthread_mutex_lock(&s->score_req_mu);
  s->score_req_stop = true;
  pthread_cond_signal(&s->score_req_cond);
  pthread_mutex_unlock(&s->score_req_mu);

  pthread_t t;
  if (pthread_create(&t, NULL, async_destroy_worker, s) == 0) {
    pthread_detach(t);
  } else {
    fzf_log("async_session_destroy_async: pthread_create failed, "
            "falling back to synchronous destroy\n");
    async_session_destroy(s);
  }
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
    emacs_value v = defcustom_value(env, Qsym_shell_file_name, Qnil);
    if (!env->eq(env, v, Qnil)) {
      ptrdiff_t slen = 0;
      env->copy_string_contents(env, v, NULL, &slen);
      if (slen > 1) {
        shell_prog = malloc((size_t)slen);
        if (shell_prog) env->copy_string_contents(env, v, shell_prog, &slen);
      }
    }
    if (!shell_prog) shell_prog = strdup("/bin/sh");
  }
  {
    emacs_value v = defcustom_value(env, Qsym_shell_command_switch, Qnil);
    if (!env->eq(env, v, Qnil)) {
      ptrdiff_t slen = 0;
      env->copy_string_contents(env, v, NULL, &slen);
      if (slen > 1) {
        shell_switch = malloc((size_t)slen);
        if (shell_switch) env->copy_string_contents(env, v, shell_switch, &slen);
      }
    }
    if (!shell_switch) shell_switch = strdup("-c");
  }

  /* Build PATH from exec-path so the child shell can find binaries that
     Emacs can find, even on macOS GUI launches with a minimal inherited PATH. */
  char *exec_path_str = NULL;
  {
    emacs_value v = defcustom_value(env, Qsym_exec_path, Qnil);
    if (!env->eq(env, v, Qnil)) {
      emacs_value sep    = env->make_string(env, ":", 1);
      emacs_value id     = env->intern(env, "identity");
      emacs_value mc_fn  = env->intern(env, "mapconcat");
      emacs_value mc_args[3] = {id, v, sep};
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
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
    }
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
  if (!s->fp) close(pfd[0]);
  /* cands_top is zero-initialized by the calloc above; blocks are
     allocated lazily by the reader on first write into each block. */
  pthread_mutex_init(&s->mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  atomic_store(&s->gen, 0);
  atomic_store(&s->score_abort, false);
  atomic_store(&s->child_owner, AsyncChildUnclaimed);
  atomic_store(&s->producer_state, AsyncProducerRunning);
  atomic_store(&s->producer_error_kind, AsyncProducerErrorNone);
  atomic_store(&s->producer_error_number, 0);
  atomic_store(&s->producer_exit_status, -1);

  {
    /* Canonical name; fzf-async bridges `fzf-async-max-line-length'
       onto this via :around advice on `fzf-native-async-start'.
       Type is integer (positive = exclude, negative = truncate) or nil
       (no limit).  The defcustom default lives in fzf-native.el — no
       hardcoded fallback here. */
    emacs_value val = defcustom_value(env, Qsym_max_line_length, Qnil);
    if (!env->eq(env, val, Qnil)) {
      s->max_line_length = (ptrdiff_t)env->extract_integer(env, val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        env->non_local_exit_clear(env);
        s->max_line_length = 0;
      }
    }
  }

  {
    size_t cache_max = 40;
    /* Canonical name; fzf-async bridges `fzf-async-cache-size'
       onto this via :around advice on `fzf-native-async-start'. */
    emacs_value val = defcustom_value(env, Qsym_async_cache_size, Qnil);
    if (!env->eq(env, val, Qnil)) {
      intmax_t n = env->extract_integer(env, val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n > 0)
        cache_max = (size_t)n;
    }
    cache_init(&s->cache, cache_max);
  }

  {
    size_t batch_cache_bytes = 64 * 1024 * 1024;
    emacs_value val = defcustom_value(
        env, Qsym_async_batch_cache_bytes, Qnil);
    if (!env->eq(env, val, Qnil)) {
      intmax_t n = env->extract_integer(env, val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n >= 0)
        batch_cache_bytes = (size_t)n;
    }
    batch_cache_init(&s->batch_cache, batch_cache_bytes);
  }

  /* Filter-only threshold; nil or <= 0 disables. */
  {
    emacs_value val = defcustom_value(env, Qsym_filter_only_min_pool, Qnil);
    if (!env->eq(env, val, Qnil)) {
      intmax_t n = env->extract_integer(env, val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n > 0)
        s->filter_only_min_pool = (size_t)n;
    }
  }

  long detected_cpus = sysconf(_SC_NPROCESSORS_ONLN);
  unsigned worker_count = detected_cpus > 0 ? (unsigned)detected_cpus : 1;
  if (worker_count > ASYNC_WORKER_LIMIT) worker_count = ASYNC_WORKER_LIMIT;
  s->worker_pool = async_worker_pool_create(worker_count);

  bool start_ok = s->fp != NULL && s->worker_pool != NULL;
  if (start_ok) {
    start_ok = pthread_create(&s->reader, NULL, async_reader, s) == 0;
    s->reader_started = start_ok;
  }
  if (start_ok) {
    start_ok = pthread_create(&s->score_thread, NULL,
                              scoring_thread_fn, s) == 0;
    s->score_thread_started = start_ok;
  }
  if (!start_ok) {
    async_session_destroy(s);
    return Qnil;
  }
  /* The user_ptr finalizer (GC sweep on Emacs main thread) routes through
     the async path too: signaling + pthread_create are O(µs), so GC stays
     fast and the blocking pthread_join runs off-main. */
  return env->make_user_ptr(env, async_session_destroy_async, s);
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
    async_session_destroy_async(s);
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
  size_t batch_id;
  bool cacheable;
  ScoredStr xs[BATCH_SIZE];
};

struct AsyncScoringShared {
  fzf_pattern_t            *pattern;
  struct AsyncScoringBatch *batches;
  _Atomic ssize_t           remaining;
  _Atomic bool             *stop;     /* points to session's score_abort */
  _Atomic size_t           *progress_completed;
  BatchCache               *batch_cache;
  BatchQuery               *target_query;
  /* When true, workers replace fzf_get_score with fzf_has_match (boolean
     match-only check from fzf-additions).  The compaction logic is
     identical; the score field is just set to 0 (unscored) and the
     calling thread skips counting_sort_scored. */
  bool                      filter_only;
};

struct AsyncWorkerContext;

struct AsyncWorkerPool {
  pthread_t *threads;
  struct AsyncWorkerContext *contexts;
  unsigned count;
  pthread_mutex_t mu;
  pthread_cond_t job_cond;
  pthread_cond_t done_cond;
  bool stop;
  uint64_t epoch;
  unsigned active;
  struct AsyncScoringShared *job;
};

struct AsyncWorkerContext {
  struct AsyncWorkerPool *pool;
};

static void async_score_batches(struct AsyncScoringShared *shared,
                                fzf_slab_t *slab) {
  fzf_pattern_t *pattern      = shared->pattern;
  bool           filter_only  = shared->filter_only;

  ssize_t bi;
  while ((bi = atomic_fetch_sub_explicit(&shared->remaining, 1,
                                         memory_order_seq_cst) - 1) >= 0) {
    if (shared->stop && atomic_load_explicit(shared->stop, memory_order_relaxed))
      break;
    struct AsyncScoringBatch *batch = shared->batches + bi;
    unsigned original_len = batch->len;
    unsigned n = 0;
    bool aborted = false;
    for (unsigned i = 0; i < batch->len; i++) {
      if ((i & 0xFF) == 0 && shared->stop &&
          atomic_load_explicit(shared->stop, memory_order_relaxed)) {
        aborted = true; break;
      }
      int sc;
      if (!pattern) {
        sc = 1;                  /* empty filter: keep everything */
      } else if (filter_only) {
        sc = fzf_has_match(batch->xs[i].str, pattern, slab) ? 1 : 0;
      } else {
        sc = fzf_get_score(batch->xs[i].str, pattern, slab);
      }
      if (!pattern || sc > 0) {
        batch->xs[n]         = batch->xs[i];
        batch->xs[n++].score = sc;
      }
    }
    if (aborted) break;
    batch->len = n;
    if (batch->cacheable && shared->batch_cache && shared->target_query)
      batch_cache_insert(shared->batch_cache, shared->target_query,
                         batch->batch_id, batch->xs, batch->len);
    if (shared->progress_completed)
      atomic_fetch_add_explicit(shared->progress_completed, original_len,
                                memory_order_relaxed);
  }
}

static void *async_persistent_worker(void *ptr) {
  fzf_block_all_signals();
  struct AsyncWorkerContext *context = ptr;
  struct AsyncWorkerPool *pool = context->pool;
  uint64_t seen_epoch = 0;
  /* Each worker owns one slab for its full session lifetime. */
  fzf_slab_t *slab = fzf_make_default_slab();

  pthread_mutex_lock(&pool->mu);
  for (;;) {
    while (!pool->stop && pool->epoch == seen_epoch)
      pthread_cond_wait(&pool->job_cond, &pool->mu);
    if (pool->stop) break;

    struct AsyncScoringShared *job = pool->job;
    seen_epoch = pool->epoch;
    pthread_mutex_unlock(&pool->mu);

    async_score_batches(job, slab);

    pthread_mutex_lock(&pool->mu);
    if (pool->active > 0 && --pool->active == 0)
      pthread_cond_signal(&pool->done_cond);
  }
  pthread_mutex_unlock(&pool->mu);

  fzf_free_slab(slab);
  return NULL;
}

static struct AsyncWorkerPool *async_worker_pool_create(unsigned count) {
  if (count == 0) count = 1;
  struct AsyncWorkerPool *pool = calloc(1, sizeof *pool);
  if (!pool) return NULL;
  pool->threads = calloc(count, sizeof *pool->threads);
  pool->contexts = calloc(count, sizeof *pool->contexts);
  if (!pool->threads || !pool->contexts) {
    free(pool->threads);
    free(pool->contexts);
    free(pool);
    return NULL;
  }
  pthread_mutex_init(&pool->mu, NULL);
  pthread_cond_init(&pool->job_cond, NULL);
  pthread_cond_init(&pool->done_cond, NULL);

  for (unsigned i = 0; i < count; i++) {
    pool->contexts[i].pool = pool;
    if (pthread_create(&pool->threads[i], NULL,
                       async_persistent_worker,
                       &pool->contexts[i]) != 0)
      break;
    pool->count++;
  }
  if (pool->count == 0) {
    async_worker_pool_destroy(pool);
    return NULL;
  }
  return pool;
}

static void async_worker_pool_run(struct AsyncWorkerPool *pool,
                                  struct AsyncScoringShared *job) {
  if (!pool || !job || pool->count == 0) return;
  pthread_mutex_lock(&pool->mu);
  pool->job = job;
  pool->active = pool->count;
  pool->epoch++;
  pthread_cond_broadcast(&pool->job_cond);
  while (pool->active > 0)
    pthread_cond_wait(&pool->done_cond, &pool->mu);
  pool->job = NULL;
  pthread_mutex_unlock(&pool->mu);
}

static void async_worker_pool_destroy(struct AsyncWorkerPool *pool) {
  if (!pool) return;
  pthread_mutex_lock(&pool->mu);
  pool->stop = true;
  pthread_cond_broadcast(&pool->job_cond);
  pthread_mutex_unlock(&pool->mu);
  for (unsigned i = 0; i < pool->count; i++)
    pthread_join(pool->threads[i], NULL);
  pthread_cond_destroy(&pool->done_cond);
  pthread_cond_destroy(&pool->job_cond);
  pthread_mutex_destroy(&pool->mu);
  free(pool->contexts);
  free(pool->threads);
  free(pool);
}

static void async_clear_current_request(AsyncSession *s, uint64_t request_id) {
  pthread_mutex_lock(&s->score_req_mu);
  if (s->score_current_id == request_id) {
    s->score_current_id = 0;
    free(s->score_current_filter);
    s->score_current_filter = NULL;
  }
  pthread_mutex_unlock(&s->score_req_mu);
}

/* Convert one coalesced reader-growth event into a retry of the latest logical
   request.  The request ID stays stable because the user did not change the
   query.  The prior completed result remains public until this retry finishes. */
static bool async_queue_growth_retry(AsyncSession *s) {
  uint64_t request_id = 0;
  char *filter = NULL;
  size_t limit = 0, fo_max_len = 0;
  fzf_case_types case_mode = CaseSmart;
  bool fuzzy = true, fo_logic_and = false;

  pthread_mutex_lock(&s->score_req_mu);
  if (s->score_req_filter || s->score_current_filter ||
      !atomic_exchange_explicit(&s->score_growth_pending, false,
                                memory_order_acq_rel)) {
    pthread_mutex_unlock(&s->score_req_mu);
    return false;
  }
  if (s->score_latest_id != 0 && s->score_latest_filter) {
    request_id = s->score_latest_id;
    filter = strdup(s->score_latest_filter);
    limit = s->score_latest_limit;
    case_mode = s->score_latest_case_mode;
    fuzzy = s->score_latest_fuzzy;
    fo_max_len = s->score_latest_filter_only_length;
    fo_logic_and = s->score_latest_filter_only_logic_and;
  }
  pthread_mutex_unlock(&s->score_req_mu);
  if (!filter) return false;

  pthread_mutex_lock(&s->mu);
  size_t current_pool = s->count;
  pthread_mutex_unlock(&s->mu);

  pthread_mutex_lock(&s->score_res_mu);
  uint64_t result_id = s->score_result_id;
  size_t result_pool_gen = s->score_result_pool_gen;
  pthread_mutex_unlock(&s->score_res_mu);

  /* The event can describe a candidate that the previous attempt already
     included.  Do not rescan an unchanged authoritative boundary. */
  if (result_id == request_id && result_pool_gen >= current_pool) {
    free(filter);
    return false;
  }

  SharedIdx *refine_idx = NULL;
  size_t refine_delta_from = 0;
  bool cache_hit = cache_lookup_membership_exact(
      &s->cache, filter, case_mode, fuzzy,
      &refine_idx, &refine_delta_from);
  if (cache_hit && refine_delta_from > current_pool) {
    shared_idx_release(refine_idx);
    refine_idx = NULL;
    refine_delta_from = 0;
  }

  pthread_mutex_lock(&s->score_req_mu);
  bool still_latest = !s->score_req_stop && !s->score_req_filter &&
                      !s->score_current_filter &&
                      s->score_latest_id == request_id;
  if (still_latest) {
    s->score_req_id = request_id;
    s->score_req_filter = filter;
    s->score_req_limit = limit;
    s->score_req_case_mode = case_mode;
    s->score_req_fuzzy = fuzzy;
    s->score_req_refine_idx = refine_idx;
    s->score_req_refine_delta_from = refine_delta_from;
    s->score_req_filter_only_length = fo_max_len;
    s->score_req_filter_only_logic_and = fo_logic_and;
    pthread_cond_signal(&s->score_req_cond);
  }
  pthread_mutex_unlock(&s->score_req_mu);

  if (!still_latest) {
    free(filter);
    shared_idx_release(refine_idx);
  }
  fzf_log("async_growth_retry: id=%llu pool=%zu result_pool=%zu refine=%d\n",
          (unsigned long long)request_id, current_pool, result_pool_gen,
          cache_hit ? 1 : 0);
  return still_latest;
}

static void *scoring_thread_fn(void *arg) {
  fzf_block_all_signals();
  AsyncSession *s = arg;
  fzf_log("scoring_thread START\n");

  for (;;) {
    pthread_mutex_lock(&s->score_req_mu);
    while (!s->score_req_stop && !s->score_req_filter &&
           !atomic_load_explicit(&s->score_growth_pending,
                                 memory_order_acquire))
      pthread_cond_wait(&s->score_req_cond, &s->score_req_mu);
    if (s->score_req_stop) {
      pthread_mutex_unlock(&s->score_req_mu);
      break;
    }
    if (!s->score_req_filter) {
      pthread_mutex_unlock(&s->score_req_mu);
      async_queue_growth_retry(s);
      continue;
    }
    /* This attempt snapshots all growth that was visible before its pool
       boundary.  A later append changes the flag back to true. */
    atomic_store_explicit(&s->score_growth_pending, false,
                          memory_order_release);
    uint64_t        request_id       = s->score_req_id;
    char           *filter           = s->score_req_filter;       /* steal ownership */
    size_t          limit            = s->score_req_limit;
    fzf_case_types  case_mode        = s->score_req_case_mode;
    bool            fuzzy            = s->score_req_fuzzy;
    SharedIdx      *refine_idx       = s->score_req_refine_idx;   /* steal */
    size_t          refine_delta_from = s->score_req_refine_delta_from;
    size_t          fo_max_len       = s->score_req_filter_only_length;
    bool            fo_logic_and     = s->score_req_filter_only_logic_and;
    s->score_req_id          = 0;
    s->score_req_filter      = NULL;
    s->score_req_refine_idx  = NULL;
    /* Record what we're about to score so main thread can skip abort for same filter */
    free(s->score_current_filter);
    s->score_current_id = request_id;
    s->score_current_filter = strdup(filter);
    s->score_current_limit  = limit;
    s->score_current_case_mode = case_mode;
    s->score_current_fuzzy = fuzzy;
    s->score_current_filter_only_length = fo_max_len;
    s->score_current_filter_only_logic_and = fo_logic_and;
    atomic_store_explicit(&s->score_progress_completed, 0,
                          memory_order_relaxed);
    atomic_store_explicit(&s->score_progress_total, 0,
                          memory_order_relaxed);
    /* Reset while holding score_req_mu.  A later submit cannot set abort
       until after this request is fully installed as current. */
    atomic_store_explicit(&s->score_abort, false, memory_order_seq_cst);
    pthread_mutex_unlock(&s->score_req_mu);

    if (!s->score_current_filter) {
      shared_idx_release(refine_idx);
      async_publish_score_failure(
          s, request_id, "matcher could not retain the current query");
      free(filter);
      continue;
    }

    /* Snapshot the append-only pool boundary.  Candidate strings and published
       pointer blocks remain immutable for the session lifetime. */
    pthread_mutex_lock(&s->mu);
    size_t count = s->count;
    pthread_mutex_unlock(&s->mu);
    shared_idx_release(refine_idx);
    refine_idx = NULL;
    (void)refine_delta_from;

    /* Decide filter-only vs full-scoring mode for this run.  In
       filter-only the workers use fzf_has_match (cheap boolean check
       from fzf-additions) and we skip top-K sorting at the end.
       Match-set is still built exhaustively (m_idx) so prefix
       refinement on the next keystroke is correct.  Pool order is
       preserved in the emit list since no sort runs.

       The min-pool arm is cached at session start; the length arm and
       the logic knob were snapshot per dispatch on the main thread and
       carried here via the request slot. */
    size_t filter_byte_len = filter ? strlen(filter) : 0;
    size_t flen_for_decision = filter
                                   ? utf8_character_count(filter, filter_byte_len)
                                   : 0;
    bool   filter_only_mode  = decide_filter_only(
        s->filter_only_min_pool, fo_max_len, fo_logic_and,
        flen_for_decision, count);
    fzf_log("scoring_thread: filter_only=%d (min_pool=%zu max_len=%zu logic=%s qlen=%zu count=%zu)\n",
            (int)filter_only_mode, s->filter_only_min_pool, fo_max_len,
            fo_logic_and ? "and" : "or", flen_for_decision, count);

    /* Select one safe cached ancestor for this query.  Each immutable full
       batch can reuse that ancestor independently; a cache miss scans the
       full batch.  The mutable final partial batch always scans in full. */
    BatchQuery *source_query = batch_cache_select_source(
        &s->batch_cache, filter, case_mode, fuzzy);
    BatchQuery *target_query = batch_cache_acquire_query(
        &s->batch_cache, filter, case_mode, fuzzy);
    size_t batch_count = count ? (count + BATCH_SIZE - 1) / BATCH_SIZE : 0;
    struct AsyncScoringBatch *batches = batch_count
        ? calloc(batch_count, sizeof *batches) : NULL;
    bool batch_ok = true;
    size_t scan_count = 0;
    size_t reused_batches = 0;
    if (batch_count && !batches) batch_ok = false;
    for (size_t bi = 0; batch_ok && bi < batch_count; bi++) {
      if (atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
        batch_ok = false;
        break;
      }
      size_t start = bi * BATCH_SIZE;
      size_t available = MIN(BATCH_SIZE, count - start);
      bool full_stable_batch = available == BATCH_SIZE;
      uint16_t cached_local[BATCH_SIZE];
      size_t selected_count = 0;
      bool reused = full_stable_batch && source_query &&
          batch_cache_copy_members(&s->batch_cache, source_query, bi,
                                   cached_local, &selected_count);
      if (!reused) selected_count = available;

      struct AsyncScoringBatch *batch = &batches[bi];
      batch->batch_id = bi;
      batch->cacheable = full_stable_batch;
      batch->len = (unsigned)selected_count;
      pthread_mutex_lock(&s->mu);
      for (size_t local_i = 0; local_i < selected_count; local_i++) {
        size_t local = reused ? cached_local[local_i] : local_i;
        size_t global_i = start + local;
        batch->xs[local_i].str =
            s->cands_top[global_i >> CANDS_BLOCK_SHIFT]
                        [global_i & CANDS_BLOCK_MASK];
        batch->xs[local_i].score = 0;
        batch->xs[local_i].idx = (uint32_t)global_i;
      }
      pthread_mutex_unlock(&s->mu);
      scan_count += selected_count;
      if (reused) reused_batches++;
    }
    if (!batch_ok) {
      bool aborted = atomic_load_explicit(
          &s->score_abort, memory_order_relaxed);
      async_clear_current_request(s, request_id);
      free(filter); free(batches);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      if (!aborted)
        async_publish_score_failure(
            s, request_id, "matcher could not allocate batch storage");
      continue;
    }
    atomic_store_explicit(&s->score_progress_total, scan_count,
                          memory_order_relaxed);

    unsigned num_batches = (unsigned)batch_count;

    size_t flen = strlen(filter);
    char *pattern_query = flen ? strdup(filter) : NULL;
    if (flen && !pattern_query) {
      async_publish_score_failure(
          s, request_id, "matcher could not copy the query for parsing");
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }
    fzf_pattern_t *pattern = flen
        ? fzf_parse_pattern(case_mode, false, pattern_query, fuzzy)
        : NULL;
    free(pattern_query);
    bool has_pattern = (pattern != NULL);

    struct AsyncScoringShared shared = {
      .pattern     = pattern,
      .batches     = batches,
      .remaining   = num_batches,
      .stop        = &s->score_abort,
      .progress_completed = &s->score_progress_completed,
      .batch_cache = &s->batch_cache,
      .target_query = target_query,
      .filter_only = filter_only_mode,
    };

    if (num_batches)
      async_worker_pool_run(s->worker_pool, &shared);
    /* `pattern' lifetime extends past the worker-pool rendezvous.  In filter-only
       mode we re-score the emit window below to recover ranked order
       within the displayed top-K.  Freed once we're done with both. */

    /* If a different filter arrived while we were scoring, discard partial results. */
    if (atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
      async_clear_current_request(s, request_id);
      if (pattern) fzf_free_pattern(pattern);
      free(batches); free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }

    /* Compact into flat array */
    size_t total = 0;
    for (unsigned i = 0; i < num_batches; i++) total += batches[i].len;

    ScoredStr *flat = total ? malloc(total * sizeof *flat) : NULL;
    if (total && !flat) {
      if (pattern) fzf_free_pattern(pattern);
      async_publish_score_failure(
          s, request_id, "matcher could not allocate result storage");
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }
    size_t pos = 0;
    if (flat) {
      for (unsigned i = 0; i < num_batches; i++) {
        struct AsyncScoringBatch *b = batches + i;
        for (unsigned j = 0; j < b->len; j++)
          flat[pos++] = b->xs[j];
      }
      /* Full-mode sorts the entire match-set by score.  Filter-only mode
         skips this — every match got score=1 from fzf_has_match so the
         sort would be a no-op.  Ranking inside the emit window is
         restored below by re-scoring just those entries with
         fzf_get_score and sorting them. */
      if (has_pattern && !filter_only_mode && pos > 1)
        counting_sort_scored(flat, pos);
    }

    size_t emit = (limit && limit < pos) ? limit : pos;

    /* Filter-only display ordering: re-score and sort the emit window so
       the user sees best-of-emit first (not pool order).  Cost is
       bounded by `emit' (== `limit', typically 10K) — about 5ms total —
       not pool size, so the big-pool savings from fzf_has_match are
       preserved.  Match-set m_idx (built below from all `pos' matches)
       stays exhaustive: order within the set doesn't affect set
       membership, which is all refinement needs. */
    if (filter_only_mode && has_pattern && flat && emit > 1) {
      fzf_slab_t *rank_slab = fzf_make_default_slab();
      for (size_t i = 0; i < emit; i++)
        flat[i].score = fzf_get_score(flat[i].str, pattern, rank_slab);
      if (rank_slab) fzf_free_slab(rank_slab);
      counting_sort_scored(flat, emit);
    }
    if (pattern) fzf_free_pattern(pattern);

    /* Build matched_idx array (all pos matches, not just top-K) for the
       cache so a future subsuming query can refine-score only this set. */
    uint32_t *m_idx_buf = (pos && flat) ? malloc(pos * sizeof *m_idx_buf) : NULL;
    if (m_idx_buf) for (size_t k = 0; k < pos; k++) m_idx_buf[k] = flat[k].idx;

    /* Cache the result.  pool_gen = count (the pool size we actually scored).
       For refine runs, count may be > refine_delta_from, so the new entry
       supersedes the old one as a refinement source for the same query. */
    cache_insert_for_request(&s->cache, filter, count, case_mode, fuzzy,
                             filter_only_mode, flat, emit, pos,
                             m_idx_buf, pos);
    free(m_idx_buf);

    /* Cache completed work even when a newer query arrived during final
       compaction.  Hold the request lock through publication so a submit
       cannot supersede this request after the authority check but before the
       result swap.  The request -> result lock order is used nowhere in
       reverse. */
    char *result_filter = strdup(filter);
    if (!result_filter) {
      free(flat);
      async_publish_score_failure(
          s, request_id, "matcher could not retain the result query");
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }
    pthread_mutex_lock(&s->score_req_mu);
    bool authoritative = request_id == s->score_latest_id &&
                         !atomic_load_explicit(&s->score_abort,
                                               memory_order_relaxed);

    if (authoritative) {
      pthread_mutex_lock(&s->score_res_mu);
      free(s->score_results);
      free(s->score_result_filter);
      s->score_results = flat;
      s->score_count   = emit;
      s->score_result_id = request_id;
      s->score_result_filter = result_filter;
      s->score_result_limit = limit;
      s->score_result_case_mode = case_mode;
      s->score_result_fuzzy = fuzzy;
      s->score_result_filter_only = filter_only_mode;
      s->score_result_pool_gen = count;
      s->score_snapshot_generation++;
      s->score_result_progress_completed = atomic_load_explicit(
          &s->score_progress_completed, memory_order_relaxed);
      s->score_result_progress_total = atomic_load_explicit(
          &s->score_progress_total, memory_order_relaxed);
      if (s->score_error_id == request_id) {
        free(s->score_error);
        s->score_error = NULL;
        s->score_error_id = 0;
      }
      s->last_filtered = pos;
      s->last_total    = count;
      pthread_mutex_unlock(&s->score_res_mu);

      /* Increment gen so compatibility callers know results changed. */
      atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
    } else {
      free(flat);
      free(result_filter);
    }
    if (s->score_current_id == request_id) {
      s->score_current_id = 0;
      free(s->score_current_filter);
      s->score_current_filter = NULL;
    }
    pthread_mutex_unlock(&s->score_req_mu);

    fzf_log("scoring_thread: filter='%s' filtered=%zu total=%zu emit=%zu "
            "reused_batches=%zu scanned=%zu\n",
            filter, pos, count, emit, reused_batches, scan_count);
    (void)reused_batches;

    batch_cache_release_query(&s->batch_cache, source_query);
    batch_cache_release_query(&s->batch_cache, target_query);
    free(batches);
    free(filter);
  }

  fzf_log("scoring_thread EXIT\n");
  return NULL;
}

static bool async_request_matches(const char *stored_filter,
                                  size_t stored_limit,
                                  fzf_case_types stored_case_mode,
                                  bool stored_fuzzy,
                                  size_t stored_fo_length,
                                  bool stored_fo_logic_and,
                                  const char *filter, size_t limit,
                                  fzf_case_types case_mode, bool fuzzy,
                                  size_t fo_length, bool fo_logic_and) {
  return stored_filter && strcmp(stored_filter, filter) == 0 &&
         stored_limit == limit && stored_case_mode == case_mode &&
         stored_fuzzy == fuzzy && stored_fo_length == fo_length &&
         stored_fo_logic_and == fo_logic_and;
}

/* A running request can be reused only while it is still the latest request
   and no replacement is queued.  If A is running, B is queued, and the user
   changes back to A, that new A must replace B with a new request ID. */
static bool async_current_request_reusable(
    const AsyncSession *s, const char *filter, size_t limit,
    fzf_case_types case_mode, bool fuzzy,
    size_t fo_length, bool fo_logic_and) {
  return s->score_req_id == 0 &&
         s->score_current_id == s->score_latest_id &&
         async_request_matches(
             s->score_current_filter, s->score_current_limit,
             s->score_current_case_mode, s->score_current_fuzzy,
             s->score_current_filter_only_length,
             s->score_current_filter_only_logic_and,
             filter, limit, case_mode, fuzzy, fo_length, fo_logic_and);
}

/* Plain-C request entry used by the Emacs wrapper and the interactive
   libFuzzer target.  Callers resolve the dynamic matching settings before
   entering here; FILTER ownership always transfers to this function. */
static uint64_t async_submit_request_resolved(
    AsyncSession *s, char *filter, size_t filter_byte_len, size_t limit,
    fzf_case_types case_mode, bool fuzzy, size_t fo_max_len,
    bool fo_logic_and) {
  if (!filter) return 0;

  pthread_mutex_lock(&s->mu);
  size_t current_pool = s->count;
  pthread_mutex_unlock(&s->mu);

  size_t filter_char_len = utf8_character_count(filter, filter_byte_len);
  bool requested_filter_only = decide_filter_only(
      s->filter_only_min_pool, fo_max_len, fo_logic_and,
      filter_char_len, current_pool);

  /* Timer-driven compatibility callers repeatedly submit the same query.
     Return the existing request ID without modifying its cancellation state. */
  pthread_mutex_lock(&s->score_req_mu);
  if (async_request_matches(
          s->score_req_filter, s->score_req_limit,
          s->score_req_case_mode, s->score_req_fuzzy,
          s->score_req_filter_only_length,
          s->score_req_filter_only_logic_and,
          filter, limit, case_mode, fuzzy, fo_max_len, fo_logic_and)) {
    uint64_t id = s->score_req_id;
    pthread_mutex_unlock(&s->score_req_mu);
    free(filter);
    return id;
  }
  if (async_current_request_reusable(
          s, filter, limit, case_mode, fuzzy, fo_max_len, fo_logic_and)) {
    uint64_t id = s->score_current_id;
    pthread_mutex_unlock(&s->score_req_mu);
    free(filter);
    return id;
  }
  pthread_mutex_unlock(&s->score_req_mu);

  ScoredStr *cached_top = NULL;
  size_t cached_count = 0;
  SharedIdx *cached_m_idx = NULL;
  size_t cached_pool_gen = 0;
  size_t cached_matched_count = 0;
  bool cached_result_covered = false;

  bool exact_hit = cache_lookup_exact_for_request(
      &s->cache, filter, case_mode, fuzzy,
      requested_filter_only, limit,
      &cached_top, &cached_count,
      &cached_m_idx, &cached_pool_gen,
      &cached_matched_count, &cached_result_covered);
  bool prefix_hit = false;
  if (!exact_hit)
    prefix_hit = cache_lookup_prefix(&s->cache, filter, case_mode, fuzzy,
                                     &cached_top, &cached_count,
                                     &cached_m_idx, &cached_pool_gen);

  bool exact_fresh = exact_hit && cached_result_covered &&
                     cached_pool_gen == current_pool;
  char *latest_filter = strdup(filter);

  pthread_mutex_lock(&s->score_req_mu);
  uint64_t request_id = ++s->score_next_id;
  s->score_latest_id = request_id;
  free(s->score_latest_filter);
  s->score_latest_filter = latest_filter;
  s->score_latest_limit = limit;
  s->score_latest_case_mode = case_mode;
  s->score_latest_fuzzy = fuzzy;
  s->score_latest_filter_only_length = fo_max_len;
  s->score_latest_filter_only_logic_and = fo_logic_and;
  atomic_store_explicit(&s->score_has_request, true, memory_order_release);

  bool current_changed = s->score_current_filter &&
      !async_request_matches(
          s->score_current_filter, s->score_current_limit,
          s->score_current_case_mode, s->score_current_fuzzy,
          s->score_current_filter_only_length,
          s->score_current_filter_only_logic_and,
          filter, limit, case_mode, fuzzy, fo_max_len, fo_logic_and);
  if (current_changed)
    atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);

  free(s->score_req_filter);
  s->score_req_filter = NULL;
  shared_idx_release(s->score_req_refine_idx);
  s->score_req_refine_idx = NULL;
  s->score_req_id = 0;

  if (!exact_fresh) {
    s->score_req_id                = request_id;
    s->score_req_filter            = filter;
    s->score_req_limit             = limit;
    s->score_req_case_mode         = case_mode;
    s->score_req_fuzzy             = fuzzy;
    s->score_req_refine_idx        = cached_m_idx;
    s->score_req_refine_delta_from = cached_pool_gen;
    s->score_req_filter_only_length    = fo_max_len;
    s->score_req_filter_only_logic_and = fo_logic_and;
    pthread_cond_signal(&s->score_req_cond);
  }
  pthread_mutex_unlock(&s->score_req_mu);

  pthread_mutex_lock(&s->score_res_mu);
  s->last_total = current_pool;
  pthread_mutex_unlock(&s->score_res_mu);

  if (exact_fresh) {
    shared_idx_release(cached_m_idx);
    pthread_mutex_lock(&s->score_res_mu);
    free(s->score_results);
    free(s->score_result_filter);
    s->score_results = cached_top;
    s->score_count = cached_count;
    s->score_result_id = request_id;
    s->score_result_filter = filter;
    s->score_result_limit = limit;
    s->score_result_case_mode = case_mode;
    s->score_result_fuzzy = fuzzy;
    s->score_result_filter_only = requested_filter_only;
    s->score_result_pool_gen = current_pool;
    s->score_snapshot_generation++;
    s->score_result_progress_completed = 0;
    s->score_result_progress_total = 0;
    if (s->score_error_id == request_id) {
      free(s->score_error);
      s->score_error = NULL;
      s->score_error_id = 0;
    }
    s->last_filtered = cached_matched_count;
    s->last_total = current_pool;
    pthread_mutex_unlock(&s->score_res_mu);
    atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
  } else {
    free(cached_top);
    /* cached_m_idx ownership transferred with the request. */
  }

  fzf_log("async_submit: id=%llu limit=%zu pool=%zu hit=%s%s%s\n",
          (unsigned long long)request_id,
          limit, current_pool,
          exact_fresh ? "exact-fresh" : "",
          (exact_hit && !exact_fresh) ? "exact-refine" : "",
          prefix_hit ? "prefix" : (!exact_hit ? "miss" : ""));
  (void)prefix_hit;
  return request_id;
}

/* Submit one immutable query request from Emacs.  Identical queued or running
   work is reused so compatibility polling cannot create a duplicate scoring
   pass. */
static uint64_t async_submit_request(emacs_env *env, AsyncSession *s,
                                     char *filter, size_t filter_byte_len,
                                     size_t limit) {
  if (!filter) return 0;
  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  bool fuzzy = resolve_fzf_native_fuzzy(env);
  size_t fo_unused_min = 0, fo_max_len = 0;
  bool fo_logic_and = resolve_filter_only_settings(
      env, &fo_unused_min, &fo_max_len);
  (void)fo_unused_min;
  return async_submit_request_resolved(
      s, filter, filter_byte_len, limit, case_mode, fuzzy, fo_max_len,
      fo_logic_and);
}

static ScoredStr *async_copy_public_result(
    AsyncSession *s, size_t *out_count, uint64_t *out_request_id,
    char **out_filter, size_t *out_limit,
    fzf_case_types *out_case_mode, bool *out_fuzzy,
    bool *out_filter_only, size_t *out_pool_gen,
    uint64_t *out_snapshot_generation,
    size_t *out_progress_completed, size_t *out_progress_total,
    uint64_t *out_error_id, char **out_error,
    size_t *out_filtered, size_t *out_total) {
  pthread_mutex_lock(&s->score_res_mu);
  size_t count = s->score_count;
  ScoredStr *copy = count ? malloc(count * sizeof *copy) : NULL;
  if (copy && s->score_results)
    memcpy(copy, s->score_results, count * sizeof *copy);
  else if (count)
    count = 0;
  char *filter = s->score_result_filter
                     ? strdup(s->score_result_filter)
                     : NULL;
  *out_count = count;
  *out_request_id = s->score_result_id;
  *out_filter = filter;
  *out_limit = s->score_result_limit;
  *out_case_mode = s->score_result_case_mode;
  *out_fuzzy = s->score_result_fuzzy;
  *out_filter_only = s->score_result_filter_only;
  *out_pool_gen = s->score_result_pool_gen;
  *out_snapshot_generation = s->score_snapshot_generation;
  *out_progress_completed = s->score_result_progress_completed;
  *out_progress_total = s->score_result_progress_total;
  *out_error_id = s->score_error_id;
  *out_error = s->score_error ? strdup(s->score_error) : NULL;
  *out_filtered = s->last_filtered;
  *out_total = s->last_total;
  pthread_mutex_unlock(&s->score_res_mu);
  return copy;
}

static emacs_value async_build_candidate_list(emacs_env *env,
                                               const ScoredStr *snap,
                                               size_t count,
                                               const char *filter,
                                               fzf_case_types case_mode,
                                               bool fuzzy) {
  size_t hl_cap = 0;
  fzf_pattern_t *hl_pattern = NULL;
  fzf_slab_t *hl_slab = NULL;

  if (filter && *filter) {
    emacs_value hi = defcustom_value(env, Qsym_async_highlight, Qnil);
    if (env->eq(env, hi, Qt))
      hl_cap = count;
    else if (!env->eq(env, hi, Qnil)) {
      intmax_t n = env->extract_integer(env, hi);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n > 0)
        hl_cap = (size_t)n;
    }
  }

  emacs_value hl_hook = Qnil;
  HlScratch hl_scratch = { 0 };
  if (hl_cap > 0) {
    char *mutable_filter = strdup(filter);
    if (mutable_filter) {
      hl_pattern = fzf_parse_pattern(case_mode, false, mutable_filter, fuzzy);
      free(mutable_filter);
    }
    hl_slab = fzf_make_default_slab();
    hl_hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
    hl_scratch_init(&hl_scratch, strlen(filter));
  }

  emacs_value result = Qnil;
  for (size_t i = count; i-- > 0;) {
    emacs_value str = env->make_string(env, snap[i].str,
                                       (ptrdiff_t)strlen(snap[i].str));
    enum emacs_funcall_exit status = env->non_local_exit_check(env);
    if (status == emacs_funcall_exit_signal) {
      env->non_local_exit_clear(env);
      continue;
    } else if (status != emacs_funcall_exit_return) {
      break;
    }

    if (hl_pattern && i < hl_cap && !env->eq(env, hl_hook, Qnil)) {
      fzf_position_t *pos = fzf_get_positions(snap[i].str,
                                              hl_pattern, hl_slab);
      dispatch_highlight_runs(env, snap[i].str, pos, str,
                              hl_hook, &hl_scratch);
      fzf_free_positions(pos);
    }

    result = env->funcall(env, Fcons, 2, (emacs_value[]){ str, result });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      break;
  }

  if (hl_pattern) fzf_free_pattern(hl_pattern);
  if (hl_slab) fzf_free_slab(hl_slab);
  hl_scratch_free(&hl_scratch);
  return result;
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

  async_submit_request(env, s, filter,
                       flen > 0 ? (size_t)flen - 1 : 0, limit);

  size_t rcount = 0, result_limit = 0, result_pool_gen = 0;
  size_t progress_completed = 0, progress_total = 0;
  size_t filtered = 0, total = 0;
  uint64_t result_request_id = 0, error_id = 0;
  uint64_t snapshot_generation = 0;
  char *result_filter = NULL, *score_error = NULL;
  fzf_case_types result_case_mode = CaseSmart;
  bool result_fuzzy = true, result_filter_only = false;
  ScoredStr *snap = async_copy_public_result(
      s, &rcount, &result_request_id, &result_filter, &result_limit,
      &result_case_mode, &result_fuzzy, &result_filter_only,
      &result_pool_gen, &snapshot_generation,
      &progress_completed, &progress_total, &error_id, &score_error,
      &filtered, &total);
  (void)result_request_id;
  (void)result_limit;
  (void)result_filter_only;
  (void)result_pool_gen;
  (void)snapshot_generation;
  (void)progress_completed;
  (void)progress_total;
  (void)error_id;
  (void)filtered;
  (void)total;

  emacs_value result = async_build_candidate_list(
      env, snap, rcount, result_filter, result_case_mode, result_fuzzy);
  free(result_filter);
  free(score_error);
  free(snap);
  return result;
}

enum AsyncRequestState {
  AsyncRequestIdle,
  AsyncRequestQueued,
  AsyncRequestRunning,
  AsyncRequestComplete,
  AsyncRequestFailed,
  AsyncRequestCancelled,
  AsyncRequestUnknown,
};

static enum AsyncRequestState async_request_state(
    uint64_t request_id, uint64_t latest_id, uint64_t queued_id,
    uint64_t running_id, uint64_t result_id, uint64_t failed_id) {
  if (request_id == 0) return AsyncRequestIdle;
  if (request_id == queued_id) return AsyncRequestQueued;
  if (request_id == running_id) return AsyncRequestRunning;
  if (request_id == result_id) return AsyncRequestComplete;
  if (request_id == failed_id) return AsyncRequestFailed;
  if (request_id <= latest_id) return AsyncRequestCancelled;
  return AsyncRequestUnknown;
}

static const char *async_request_state_name(enum AsyncRequestState state) {
  switch (state) {
    case AsyncRequestIdle:      return "idle";
    case AsyncRequestQueued:    return "queued";
    case AsyncRequestRunning:   return "running";
    case AsyncRequestComplete:  return "complete";
    case AsyncRequestFailed:    return "failed";
    case AsyncRequestCancelled: return "cancelled";
    case AsyncRequestUnknown:   return "unknown";
  }
  return "unknown";
}

static const char *async_producer_state_name(enum AsyncProducerState state) {
  switch (state) {
    case AsyncProducerRunning:  return "running";
    case AsyncProducerComplete: return "complete";
    case AsyncProducerFailed:   return "failed";
    case AsyncProducerStopped:  return "stopped";
  }
  return "failed";
}

static bool async_producer_error_message(AsyncSession *s,
                                         char *buffer, size_t capacity) {
  enum AsyncProducerErrorKind kind =
      (enum AsyncProducerErrorKind)atomic_load_explicit(
          &s->producer_error_kind, memory_order_acquire);
  int number = atomic_load_explicit(
      &s->producer_error_number, memory_order_acquire);
  switch (kind) {
    case AsyncProducerErrorNone:
      return false;
    case AsyncProducerErrorExit:
      snprintf(buffer, capacity, "producer exited with status %d", number);
      break;
    case AsyncProducerErrorSignal:
      snprintf(buffer, capacity, "producer terminated by signal %d", number);
      break;
    case AsyncProducerErrorRead:
      snprintf(buffer, capacity, "producer output read failed: %s",
               strerror(number));
      break;
    case AsyncProducerErrorAllocation:
      snprintf(buffer, capacity,
               "producer reader could not allocate candidate storage");
      break;
    case AsyncProducerErrorCapacity:
      snprintf(buffer, capacity, "producer exceeded the candidate capacity");
      break;
    case AsyncProducerErrorWait:
      snprintf(buffer, capacity, "producer wait failed: %s", strerror(number));
      break;
  }
  return true;
}

static emacs_value async_plist_put(emacs_env *env, emacs_value plist,
                                   const char *key, emacs_value value) {
  plist = env->funcall(env, Fcons, 2, (emacs_value[]){ value, plist });
  return env->funcall(env, Fcons, 2,
                      (emacs_value[]){ env->intern(env, key), plist });
}

static emacs_value async_snapshot_value(emacs_env *env, AsyncSession *s,
                                        uint64_t requested_id,
                                        bool include_candidates) {
  pthread_mutex_lock(&s->score_req_mu);
  uint64_t latest_id = s->score_latest_id;
  uint64_t queued_id = s->score_req_id;
  uint64_t running_id = s->score_current_id;
  pthread_mutex_unlock(&s->score_req_mu);
  if (requested_id == 0) requested_id = latest_id;

  size_t result_count = 0, result_limit = 0, result_pool_gen = 0;
  size_t progress_completed = 0, progress_total = 0;
  size_t filtered = 0, total = 0;
  uint64_t result_id = 0, error_id = 0;
  uint64_t snapshot_generation = 0;
  char *result_filter = NULL, *score_error = NULL;
  fzf_case_types result_case_mode = CaseSmart;
  bool result_fuzzy = true, result_filter_only = false;
  ScoredStr *result_copy = async_copy_public_result(
      s, &result_count, &result_id, &result_filter, &result_limit,
      &result_case_mode, &result_fuzzy, &result_filter_only,
      &result_pool_gen, &snapshot_generation,
      &progress_completed, &progress_total, &error_id, &score_error,
      &filtered, &total);

  pthread_mutex_lock(&s->mu);
  size_t current_pool = s->count;
  pthread_mutex_unlock(&s->mu);

  if (requested_id == running_id) {
    progress_completed = atomic_load_explicit(
        &s->score_progress_completed, memory_order_relaxed);
    progress_total = atomic_load_explicit(
        &s->score_progress_total, memory_order_relaxed);
  } else if (requested_id != result_id) {
    progress_completed = 0;
    progress_total = 0;
  }

  size_t batch_cache_entries = 0, batch_cache_bytes = 0;
  uint64_t batch_cache_hits = 0, batch_cache_misses = 0;
  uint64_t batch_cache_evictions = 0;
  batch_cache_stats(&s->batch_cache, &batch_cache_entries,
                    &batch_cache_bytes, &batch_cache_hits,
                    &batch_cache_misses, &batch_cache_evictions);
  enum AsyncProducerState producer_state =
      (enum AsyncProducerState)atomic_load_explicit(
          &s->producer_state, memory_order_acquire);
  int producer_exit_status = atomic_load_explicit(
      &s->producer_exit_status, memory_order_acquire);
  char producer_error_buffer[192];
  bool has_producer_error = async_producer_error_message(
      s, producer_error_buffer, sizeof producer_error_buffer);

  enum AsyncRequestState state = async_request_state(
      requested_id, latest_id, queued_id, running_id, result_id, error_id);
  emacs_value candidates = Qnil;
  if (include_candidates)
    candidates = async_build_candidate_list(
        env, result_copy, result_count, result_filter,
        result_case_mode, result_fuzzy);

  emacs_value plist = Qnil;
  plist = async_plist_put(
      env, plist, ":producer-error",
      has_producer_error
          ? env->make_string(env, producer_error_buffer,
                             (ptrdiff_t)strlen(producer_error_buffer))
          : Qnil);
  plist = async_plist_put(
      env, plist, ":producer-exit-status",
      producer_exit_status >= 0
          ? env->make_integer(env, (intmax_t)producer_exit_status)
          : Qnil);
  plist = async_plist_put(
      env, plist, ":producer-state",
      env->intern(env, async_producer_state_name(producer_state)));
  plist = async_plist_put(
      env, plist, ":error",
      requested_id == error_id && score_error
          ? env->make_string(env, score_error,
                             (ptrdiff_t)strlen(score_error))
          : Qnil);
  plist = async_plist_put(env, plist, ":failed-request-id",
                          env->make_integer(env, (intmax_t)error_id));
  plist = async_plist_put(env, plist, ":batch-cache-evictions",
                          env->make_integer(
                              env, (intmax_t)batch_cache_evictions));
  plist = async_plist_put(env, plist, ":batch-cache-misses",
                          env->make_integer(
                              env, (intmax_t)batch_cache_misses));
  plist = async_plist_put(env, plist, ":batch-cache-hits",
                          env->make_integer(
                              env, (intmax_t)batch_cache_hits));
  plist = async_plist_put(env, plist, ":batch-cache-bytes",
                          env->make_integer(
                              env, (intmax_t)batch_cache_bytes));
  plist = async_plist_put(env, plist, ":batch-cache-entries",
                          env->make_integer(
                              env, (intmax_t)batch_cache_entries));
  plist = async_plist_put(env, plist, ":reader-done",
      atomic_load_explicit(&s->reader_done, memory_order_acquire) ? Qt : Qnil);
  if (include_candidates)
    plist = async_plist_put(env, plist, ":candidates", candidates);
  plist = async_plist_put(env, plist, ":total",
                          env->make_integer(env, (intmax_t)total));
  plist = async_plist_put(env, plist, ":filtered",
                          env->make_integer(env, (intmax_t)filtered));
  plist = async_plist_put(env, plist, ":progress-total",
                          env->make_integer(env,
                                            (intmax_t)progress_total));
  plist = async_plist_put(env, plist, ":progress-completed",
                          env->make_integer(
                              env, (intmax_t)progress_completed));
  plist = async_plist_put(env, plist, ":filter-only",
                          result_filter_only ? Qt : Qnil);
  plist = async_plist_put(env, plist, ":fuzzy",
                          result_fuzzy ? Qt : Qnil);
  emacs_value case_symbol = env->intern(
      env, result_case_mode == CaseIgnore ? "ignore" :
           result_case_mode == CaseRespect ? "respect" : "smart");
  plist = async_plist_put(env, plist, ":case-mode", case_symbol);
  plist = async_plist_put(env, plist, ":limit",
                          env->make_integer(env, (intmax_t)result_limit));
  plist = async_plist_put(env, plist, ":query",
      result_filter
          ? env->make_string(env, result_filter,
                             (ptrdiff_t)strlen(result_filter))
          : Qnil);
  bool stale = requested_id != result_id ||
               (result_id != 0 && result_pool_gen != current_pool);
  plist = async_plist_put(env, plist, ":stale", stale ? Qt : Qnil);
  plist = async_plist_put(env, plist, ":result-pool-generation",
                          env->make_integer(env,
                                            (intmax_t)result_pool_gen));
  plist = async_plist_put(env, plist, ":snapshot-generation",
                          env->make_integer(
                              env, (intmax_t)snapshot_generation));
  plist = async_plist_put(env, plist, ":pool-generation",
                          env->make_integer(env, (intmax_t)current_pool));
  plist = async_plist_put(env, plist, ":result-request-id",
                          env->make_integer(env, (intmax_t)result_id));
  plist = async_plist_put(env, plist, ":latest-request-id",
                          env->make_integer(env, (intmax_t)latest_id));
  plist = async_plist_put(env, plist, ":state",
                          env->intern(env, async_request_state_name(state)));
  plist = async_plist_put(env, plist, ":request-id",
                          env->make_integer(env, (intmax_t)requested_id));

  free(result_filter);
  free(score_error);
  free(result_copy);
  return plist;
}

/* fzf-native-async-submit HANDLE QUERY &optional LIMIT -> request ID */
static emacs_value
fzf_native_async_submit(emacs_env *env, ptrdiff_t nargs,
                        emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;

  ptrdiff_t qlen = 0;
  if (!env->copy_string_contents(env, args[1], NULL, &qlen)) return Qnil;
  char *query = malloc((size_t)qlen);
  if (!query) return Qnil;
  if (!env->copy_string_contents(env, args[1], query, &qlen)) {
    free(query);
    return Qnil;
  }

  size_t limit = 0;
  if (nargs > 2 && !env->eq(env, args[2], Qnil)) {
    intmax_t extracted = env->extract_integer(env, args[2]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return ||
        extracted < 0) {
      env->non_local_exit_clear(env);
      free(query);
      return Qnil;
    }
    limit = (size_t)extracted;
  }

  uint64_t request_id = async_submit_request(
      env, s, query, qlen > 0 ? (size_t)qlen - 1 : 0, limit);
  return request_id ? env->make_integer(env, (intmax_t)request_id) : Qnil;
}

/* fzf-native-async-snapshot HANDLE &optional REQUEST-ID -> plist */
static emacs_value
fzf_native_async_snapshot(emacs_env *env, ptrdiff_t nargs,
                          emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;
  uint64_t request_id = 0;
  if (nargs > 1 && !env->eq(env, args[1], Qnil)) {
    intmax_t extracted = env->extract_integer(env, args[1]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return ||
        extracted < 0) {
      env->non_local_exit_clear(env);
      return Qnil;
    }
    request_id = (uint64_t)extracted;
  }
  return async_snapshot_value(env, s, request_id, true);
}

/* fzf-native-async-status HANDLE -> plist */
static emacs_value
fzf_native_async_status(emacs_env *env, ptrdiff_t UNUSED(nargs),
                        emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;
  return async_snapshot_value(env, s, 0, false);
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

/* fzf-native-async-result-fresh-p HANDLE QUERY -> t / nil

   Returns t iff the result cache holds an entry for QUERY whose
   `pool_gen' equals the current pool size AND the pool is either
   non-empty or the producer has finished streaming — i.e. scoring
   has completed for this exact query against a real (or finalized)
   pool.  In that state any value previously returned by
   `fzf-native-async-candidates' for QUERY is authoritative, including
   nil (zero matches): the cache entry exists and its top-K is empty.

   Returns nil when no cache entry exists for QUERY, when the entry's
   `pool_gen' lags the current pool (scoring is mid-refinement), when
   HANDLE is invalid, OR when the pool is empty and the producer is
   still streaming.  The last case is the post-restart warmup window:
   `pool_gen == cur_pool == 0' would otherwise pass the cache check
   trivially (nothing to score), but the producer may still emit
   candidates that change the answer, so the empty result is not yet
   authoritative.  Only once the reader thread observes EOF (or stop)
   does `reader_done' become true, at which point a still-empty pool
   IS the final answer. */
static emacs_value
fzf_native_async_result_fresh_p(emacs_env *env, ptrdiff_t UNUSED(nargs),
                                emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;

  ptrdiff_t qlen = 0;
  if (!env->copy_string_contents(env, args[1], NULL, &qlen)) return Qnil;
  char *query = malloc((size_t)qlen);
  if (!query) return Qnil;
  if (!env->copy_string_contents(env, args[1], query, &qlen)) {
    free(query);
    return Qnil;
  }

  pthread_mutex_lock(&s->mu);
  size_t cur_pool = s->count;
  pthread_mutex_unlock(&s->mu);

  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  bool fuzzy = resolve_fzf_native_fuzzy(env);
  size_t fo_unused_min = 0, fo_max_len = 0;
  bool fo_logic_and = resolve_filter_only_settings(
      env, &fo_unused_min, &fo_max_len);
  (void)fo_unused_min;
  size_t query_char_len = qlen > 1
                              ? utf8_character_count(query, (size_t)qlen - 1)
                              : 0;
  bool filter_only = decide_filter_only(
      s->filter_only_min_pool, fo_max_len, fo_logic_and,
      query_char_len, cur_pool);

  pthread_mutex_lock(&s->cache.mu);
  CacheEntry *e = cache_find_locked(&s->cache, query, case_mode, fuzzy);
  bool fresh = (e != NULL && e->pool_gen == cur_pool &&
                e->filter_only == filter_only);
  pthread_mutex_unlock(&s->cache.mu);

  /* A larger-limit or mode-changing request can refine an otherwise fresh
     cache entry.  During that work, the old top-K is not authoritative for
     the current request. */
  pthread_mutex_lock(&s->score_req_mu);
  bool same_query_in_flight =
      (s->score_req_filter &&
       strcmp(s->score_req_filter, query) == 0 &&
       s->score_req_case_mode == case_mode &&
       s->score_req_fuzzy == fuzzy) ||
      (s->score_current_filter &&
       strcmp(s->score_current_filter, query) == 0 &&
       s->score_current_case_mode == case_mode &&
       s->score_current_fuzzy == fuzzy);
  pthread_mutex_unlock(&s->score_req_mu);
  if (same_query_in_flight) fresh = false;

  /* Empty pool while the producer is still streaming: cache match is
     trivial (scoring 0 items), not authoritative. */
  if (fresh && cur_pool == 0 &&
      !atomic_load_explicit(&s->reader_done, memory_order_acquire)) {
    fresh = false;
  }

  free(query);
  return fresh ? Qt : Qnil;
}

#endif /* APPLE || linux || FreeBSD */

/* fzf-native-filter-only-p QUERY-LENGTH POOL-SIZE -> t / nil
   Single source of truth for the filter-only decision.  Exposes the
   internal `decide_filter_only' so Elisp callers (fussy, etc.) can
   take consistent code paths without re-implementing the OR/AND
   composition and disabled-arm rules. */
static emacs_value
fzf_native_filter_only_p(emacs_env *env, ptrdiff_t UNUSED(nargs),
                         emacs_value args[], void *UNUSED(data)) {
  intmax_t qlen = env->extract_integer(env, args[0]);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env); qlen = 0;
  }
  intmax_t psize = env->extract_integer(env, args[1]);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env); psize = 0;
  }

  size_t min_pool = 0, max_len = 0;
  bool   logic_and = resolve_filter_only_settings(env, &min_pool, &max_len);

  /* Negative inputs are treated as 0 — callers shouldn't pass them but
     we don't want to underflow size_t on the cast. */
  size_t qlen_u  = qlen  > 0 ? (size_t)qlen  : 0;
  size_t psize_u = psize > 0 ? (size_t)psize : 0;
  return decide_filter_only(min_pool, max_len, logic_and, qlen_u, psize_u)
         ? Qt : Qnil;
}

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

  // fzf-native-highlight-one CAND QUERY
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-highlight-one"),
      env->make_function(env, 2, 2, fzf_native_highlight_one,
                         "Return a copy of CAND with fzf match face applied for QUERY.\n"
                         "Per-candidate counterpart to `fzf-native-highlight-all'.\n"
                         "Intended for `completion-lazy-hilit-fn' callers.\n"
                         "\n"
                         "Empty QUERY returns a face-stripped copy.  No-match returns\n"
                         "an unfaced copy.  Caller's original CAND is never mutated.\n"
                         "Ignores `fzf-native-batch-highlight' (the cap is meaningless\n"
                         "for a single candidate).  Honors `fzf-native-highlight-fn'.\n"
                         "\n"
                         "\\(fn CAND QUERY)",
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
      env->intern(env, "fzf-native-async-submit"),
      env->make_function(env, 2, 3, fzf_native_async_submit,
                         "Submit QUERY to async HANDLE and return its request ID.\n"
                         "Optional LIMIT caps the completed result.  Identical queued or\n"
                         "running work reuses its existing request ID.  This call never\n"
                         "waits for scoring and does not build an Emacs candidate list.\n\n"
                         "\\(fn HANDLE QUERY &optional LIMIT)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-snapshot"),
      env->make_function(env, 1, 2, fzf_native_async_snapshot,
                         "Return a request-aware result plist for async HANDLE.\n"
                         "Optional REQUEST-ID selects the request to inspect.  Without it,\n"
                         "inspect the latest submitted request.  The plist retains the last\n"
                         "completed candidates while newer work is queued or running.\n\n"
                         "\\(fn HANDLE &optional REQUEST-ID)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-status"),
      env->make_function(env, 1, 1, fzf_native_async_status,
                         "Return request and producer status for async HANDLE.\n"
                         "This is the metadata-only counterpart to\n"
                         "`fzf-native-async-snapshot'.\n\n"
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
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-result-fresh-p"),
      env->make_function(env, 2, 2, fzf_native_async_result_fresh_p,
                         "Return non-nil when the result cache for QUERY is fresh on HANDLE.\n"
                         "Fresh means scoring has completed for QUERY at the current pool\n"
                         "size, so the most recent `fzf-native-async-candidates' return for\n"
                         "QUERY is authoritative — a nil return in that state means zero\n"
                         "matches, not in-flight.\n\n"
                         "\\(fn HANDLE QUERY)", NULL),
    });
#endif

  /* fzf-native-filter-only-p — single source of truth for the filter-only
     decision.  Reads the filter-only defcustoms and applies the OR/AND
     composition; Elisp callers (fussy etc.) use this to take consistent
     paths without re-implementing the rule. */
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-filter-only-p"),
      env->make_function(env, 2, 2, fzf_native_filter_only_p,
                         "Return non-nil when filter-only mode would fire at QUERY-LENGTH and POOL-SIZE.\n"
                         "\n"
                         "Honors `fzf-native-filter-only-min-pool',\n"
                         "`fzf-native-filter-only-length', and `fzf-native-filter-only-logic'.\n"
                         "An arm whose defcustom is nil/0 is disabled.\n\n"
                         "\\(fn QUERY-LENGTH POOL-SIZE)", NULL),
    });

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
  Fcopy_sequence = env->make_global_ref(env, env->intern(env, "copy-sequence"));
  Fsetcar = env->make_global_ref(env, env->intern(env, "setcar"));
  Faset = env->make_global_ref(env, env->intern(env, "aset"));
  Fsymbol_value = env->make_global_ref(env, env->intern(env, "symbol-value"));
  Qsym_case_mode            = env->make_global_ref(env, env->intern(env, "fzf-native-case-mode"));
  Qsym_fuzzy                = env->make_global_ref(env, env->intern(env, "fzf-native-fuzzy"));
  Qsym_batch_highlight      = env->make_global_ref(env, env->intern(env, "fzf-native-batch-highlight"));
  Qsym_async_highlight      = env->make_global_ref(env, env->intern(env, "fzf-native-async-highlight"));
  Qsym_max_line_length      = env->make_global_ref(env, env->intern(env, "fzf-native-max-line-length"));
  Qsym_async_cache_size     = env->make_global_ref(env, env->intern(env, "fzf-native-async-cache-size"));
  Qsym_async_batch_cache_bytes = env->make_global_ref(
      env, env->intern(env, "fzf-native-async-batch-cache-bytes"));
  Qsym_filter_only_min_pool = env->make_global_ref(env, env->intern(env, "fzf-native-filter-only-min-pool"));
  Qsym_filter_only_length   = env->make_global_ref(env, env->intern(env, "fzf-native-filter-only-length"));
  Qsym_filter_only_logic    = env->make_global_ref(env, env->intern(env, "fzf-native-filter-only-logic"));
  Qsym_highlight_fn         = env->make_global_ref(env, env->intern(env, "fzf-native-highlight-fn"));
  Qor      = env->make_global_ref(env, env->intern(env, "or"));
  Qand     = env->make_global_ref(env, env->intern(env, "and"));
  Qsym_shell_file_name      = env->make_global_ref(env, env->intern(env, "shell-file-name"));
  Qsym_shell_command_switch = env->make_global_ref(env, env->intern(env, "shell-command-switch"));
  Qsym_exec_path            = env->make_global_ref(env, env->intern(env, "exec-path"));
  Qvector  = env->make_global_ref(env, env->intern(env, "vector"));
  Qstring  = env->make_global_ref(env, env->intern(env, "string"));
  Qignore  = env->make_global_ref(env, env->intern(env, "ignore"));
  Qrespect = env->make_global_ref(env, env->intern(env, "respect"));
  Qstringp = env->make_global_ref(env, env->intern(env, "stringp"));
  Qwrong_type_argument = env->make_global_ref(env, env->intern(env, "wrong-type-argument"));
  Qerror   = env->make_global_ref(env, env->intern(env, "error"));
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
