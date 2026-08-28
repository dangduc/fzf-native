/* SPDX-License-Identifier: GPL-3.0-or-later */
/* strdup is POSIX (not C11); Linux glibc hides it under c11 without this. */
#if defined(__linux__) && !defined(_POSIX_C_SOURCE)
#  define _POSIX_C_SOURCE 200809L
#endif
#include <ctype.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>
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
#include <poll.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

extern char **environ;

/* Block all signals on the current thread.  Worker threads call this on
   entry so async signals (SIGCHLD, SIGIO, ...) only ever land on Emacs's
   main thread.  Otherwise Emacs's signal handler forwards via pthread_kill,
   which can recursively lock an os_unfair_lock if it fires while the worker
   is inside libsystem code — observed crash on macOS.  Synchronous faults
   (SIGSEGV/etc.) are delivered by the kernel regardless of mask. */
#if defined(FZF_NATIVE_CTEST)
static inline void fzf_block_all_signals(void) {}
#else
static inline void fzf_block_all_signals(void) {
  sigset_t s;
  sigfillset(&s);
  pthread_sigmask(SIG_BLOCK, &s, NULL);
}
#endif

/* A forked producer must not inherit Emacs's thread signal mask or ignored
   termination dispositions.  Otherwise stop can successfully send SIGTERM
   while the child remains alive indefinitely. */
static inline void fzf_prepare_forked_child_signals(void) {
  sigset_t empty;
  sigemptyset(&empty);
  sigprocmask(SIG_SETMASK, &empty, NULL);
  struct sigaction action;
  memset(&action, 0, sizeof action);
  action.sa_handler = SIG_DFL;
  sigemptyset(&action.sa_mask);
  sigaction(SIGTERM, &action, NULL);
  sigaction(SIGINT, &action, NULL);
  sigaction(SIGQUIT, &action, NULL);
  sigaction(SIGHUP, &action, NULL);
}
#else
/* Non-POSIX (Windows): no signals to block, worker entry calls become no-ops. */
static inline void fzf_block_all_signals(void) {}
#endif

#ifdef _WIN32
/* emacs-module.h declares emacs_module_init before this translation unit can
   add __declspec(dllexport), and MSVC rejects a differently decorated
   definition.  The Windows target exports the two ABI symbols with the module
   definition file instead. */
#  define EXPORT
#elif defined(__GNUC__) || defined(__clang__)
#  define EXPORT __attribute__((visibility("default")))
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
#ifndef ASYNC_BATCH_WINDOW
#define ASYNC_BATCH_WINDOW 64
#endif
#define ASYNC_WORKER_LIMIT 64
#define BATCH_CACHE_BUCKETS 4096
#define BATCH_CACHE_SPARSE_LIMIT 128

/* Turn the fallible POSIX CPU-count probe into a bounded worker count.
   WORK_ITEMS == 0 means that no per-call work cap applies (the persistent
   async pool case).  A zero or negative probe must still run one worker;
   casting it directly to unsigned can otherwise mean zero workers or a huge
   allocation. */
static unsigned fzf_worker_count(long detected_cpus, size_t work_items) {
  unsigned count = detected_cpus > 0
                       ? detected_cpus > ASYNC_WORKER_LIMIT
                             ? ASYNC_WORKER_LIMIT
                             : (unsigned)detected_cpus
                       : 1;
  if (work_items > 0 && work_items < count)
    count = (unsigned)work_items;
  return count;
}

/* Increment when the public interactive-session contract requires matching
   Elisp.  The loader checks this before it marks a bundled module usable. */
#define FZF_NATIVE_SESSION_ABI 1

EXPORT
int plugin_is_GPL_compatible;

emacs_value Qnil, Qlistofzero, Fcons, Flist, Qt;
emacs_value Fhashtablep, Fmessage, Fvectorp, Fconsp, Fcdr, Fcar, Fvconcat;
emacs_value Ffunctionp, Fsymbolp, Fsymbolname, Flength, Fnth, Fprinc, Freverse;
emacs_value Qcompletion_score, Fput_text_property, Qzero, Qone;
emacs_value Fcopy_sequence, Fsetcar, Faset, Fmultibyte_string_p;
emacs_value Fencode_coding_string, Qutf_8;
emacs_value Qface, Qcompletions_common_part;
emacs_value Fremove_text_properties, Qface_nil_plist;
emacs_value Fsymbol_value;
/* Cached defcustom name symbols — interned once at init, looked up via
   `defcustom_value' on each read.  The values themselves stay dynamic
   so user `setq' / `customize-set-variable' is respected. */
emacs_value Qsym_case_mode, Qsym_fuzzy, Qsym_batch_highlight, Qsym_async_highlight;
emacs_value Qsym_max_line_length, Qsym_async_cache_size;
emacs_value Qsym_async_cache_bytes;
emacs_value Qsym_async_batch_cache_bytes, Qsym_filter_only_min_pool;
emacs_value Qsym_filter_only_length, Qsym_filter_only_logic;
emacs_value Qsym_shell_file_name, Qsym_shell_command_switch, Qsym_exec_path;
emacs_value Qsym_highlight_fn;
/* Cached value symbols for `type-of' comparisons and signal/error names. */
emacs_value Qvector, Qstring, Qignore, Qrespect;
emacs_value Qor, Qand;
emacs_value Qstringp, Qwrong_type_argument, Qerror;

typedef void (*fzf_native_finalizer_fn)(void *);

static void slab_finalize(void *object);

static void async_signal_error(emacs_env *env, const char *message) {
  emacs_value msg = env->make_string(env, message,
                                     (ptrdiff_t)strlen(message));
  env->non_local_exit_signal(env, Qerror,
                             env->funcall(env, Flist, 1, &msg));
}

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
static void async_signal_posix_error(emacs_env *env, const char *operation,
                                     int error_number) {
  char message[256];
  snprintf(message, sizeof message, "fzf-native: %s failed: %s",
           operation, strerror(error_number > 0 ? error_number : EIO));
  async_signal_error(env, message);
}
#endif

/* Emacs user pointers carry no C type information in their payload.  Check
   the registered finalizer before casting so a slab cannot be interpreted as
   an AsyncSession (or vice versa).  Cross-kind pointers previously exposed
   fabricated metadata and could send teardown through arbitrary memory. */
static void *fzf_native_typed_user_ptr(
    emacs_env *env, emacs_value value,
    fzf_native_finalizer_fn expected_finalizer,
    const char *expected_name) {
  void *ptr = env->get_user_ptr(env, value);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return NULL;

  fzf_native_finalizer_fn actual_finalizer =
      env->get_user_finalizer(env, value);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return NULL;
  if (actual_finalizer != expected_finalizer) {
    char message[128];
    snprintf(message, sizeof message,
             "fzf-native: expected %s", expected_name);
    async_signal_error(env, message);
    return NULL;
  }
  return ptr;
}


/** An Emacs string made accessible by copying. */
struct Str { char *b; size_t len; };

static bool str_has_embedded_nul(struct Str value) {
  return value.b && memchr(value.b, '\0', value.len) != NULL;
}

static bool reject_embedded_nul(emacs_env *env, struct Str value,
                                const char *what) {
  if (!str_has_embedded_nul(value)) return false;
  char message[128];
  snprintf(message, sizeof message,
           "fzf-native: embedded NUL in %s is not supported", what);
  async_signal_error(env, message);
  return true;
}

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

#ifdef FZF_NATIVE_CTEST
static _Atomic bool copy_test_fail_bump_allocation;
#endif

static void bump_free(struct Bump *head) {
  while (head) {
    struct Bump *next = head->next;
    free(head);
    head = next;
  }
}

/** Copies the Emacs string to make its contents accessible.

    Probes the required buffer size with a NULL-buf call first, then either
    copies into remaining bump space (if it fits) or allocates a new block.
    Avoids the opportunistic "try inline, catch memory-buffer-too-small,
    retry" pattern: that signal is now caught by CATCHER_ALL_DEBUGGABLE
    on Emacs 31+, which invokes `debug' before the module handler can
    convert it to a pending exit -- so with `debug-on-error' on, the
    silent retry pops the debugger. */
static struct Str copy_valid_emacs_string(emacs_env *env, struct Bump **bump, emacs_value value) {
  ptrdiff_t len;
  if (!env->copy_string_contents(env, value, NULL, &len)) {
    /* Length probe failed (e.g., unicode-string-p on an invalid unibyte
       string). Leave the pending exit set so the caller can try the
       encode-coding-string fallback. */
    return (struct Str) { 0 };
  }

  char *buf;
  if (*bump && (*bump)->limit - (*bump)->cursor >= len) {
    /* Fits in current bump. Copy inline. */
    buf = (*bump)->cursor;
  } else {
    /* Need a new bump. Grow at least 2x the current head, and at least
       enough to fit this string plus alignment slack. */
    size_t capacity = *bump ? 2 * (size_t)((*bump)->limit - (*bump)->b) : 2048;
    if (capacity < (size_t) len) capacity = len + alignof(uint64_t) - 1;
    struct Bump *new = NULL;
#ifdef FZF_NATIVE_CTEST
    bool force_allocation_failure = atomic_exchange_explicit(
        &copy_test_fail_bump_allocation, false, memory_order_acq_rel);
#else
    bool force_allocation_failure = false;
#endif
    if (force_allocation_failure ||
        !(new = malloc(sizeof *new + capacity)))
      return (struct Str) { 0 };
    *new = (struct Bump) { .next = *bump, .cursor = new->b, .limit = new->b + capacity };
    *bump = new;
    buf = new->cursor;
  }

  if (!env->copy_string_contents(env, value, buf, &len)) {
    /* Rare: state changed between probe and copy (e.g. string mutated).
       Clear any pending exit and drop the candidate. */
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      env->non_local_exit_clear(env);
    return (struct Str) { 0 };
  }

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

  /* Only a direct-copy signal justifies representation conversion.  A bump
     allocation failure leaves no pending exit; treating that as an encoding
     failure used to recurse forever while the allocator kept returning NULL. */
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    return (struct Str) { 0 };
  env->non_local_exit_clear(env);

  emacs_value encode_args[] = { value, Qutf_8, Qt };
  emacs_value encoded = env->funcall(env, Fencode_coding_string, 3, encode_args);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    return (struct Str) { 0 };
  }

  /* The encoded value is the one and only fallback.  Do not recurse if its
     copy also fails: a second conversion cannot make progress and can turn a
     runtime failure into unbounded recursion. */
  s = copy_valid_emacs_string(env, bump, encoded);
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
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  _Atomic bool allocation_failed;
  _Atomic bool embedded_nul;
#else
  /* Windows executes this worker synchronously on the calling thread.  MSVC's
     C11 mode does not implement `_Atomic', so no synchronization is needed. */
  bool allocation_failed;
  bool embedded_nul;
#endif
};

static bool shared_allocation_failed(struct Shared *shared) {
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  return atomic_load_explicit(&shared->allocation_failed,
                              memory_order_relaxed);
#else
  return shared->allocation_failed;
#endif
}

static void shared_set_allocation_failed(struct Shared *shared) {
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  atomic_store_explicit(&shared->allocation_failed, true,
                        memory_order_relaxed);
#else
  shared->allocation_failed = true;
#endif
}

static bool shared_embedded_nul(struct Shared *shared) {
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  return atomic_load_explicit(&shared->embedded_nul, memory_order_relaxed);
#else
  return shared->embedded_nul;
#endif
}

static void shared_set_embedded_nul(struct Shared *shared) {
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  atomic_store_explicit(&shared->embedded_nul, true, memory_order_relaxed);
#else
  shared->embedded_nul = true;
#endif
}

static bool shared_worker_should_stop(struct Shared *shared) {
  return shared_allocation_failed(shared);
}

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
    bool invalid_candidate = false;

    /* A peer OOM is terminal.  Check once per immutable batch so the
       exceptional cross-worker stop does not add an atomic load and branch
       to every ordinary candidate.  The worker that observes its own OOM
       still stops immediately below; peers finish at most one 2048-row
       batch before seeing the shared failure. */
    if (shared_worker_should_stop(shared))
      break;

    if (pattern) {
      for (unsigned i = 0; i < batch->len; ++i) {
        struct Candidate x = batch->xs[i];
        /* Validate candidate bytes on the worker that already owns this
           batch.  NUL is an invalid-input exception: publish it with a
           write-only relaxed flag and stop only this worker.  Other workers
           may finish, but ordinary candidates pay no shared-state read. */
        if (str_has_embedded_nul(x.s)) {
          shared_set_embedded_nul(shared);
          invalid_candidate = true;
          break;
        }
        /* You can get the score/position for as many items as you want */
        int score = filter_only
          ? (fzf_has_match(x.s.b, pattern, slab) ? 1 : 0)
          : fzf_get_score(x.s.b, pattern, slab);
        if (fzf_allocation_failed()) {
          shared_set_allocation_failed(shared);
          break;
        }
        if (score > 0) {
          /* printf("Str: %s # = %d | i = %d, batch->len = %d, batch_idx = %zd\n", */
          /*        x.s.b, score, i, batch->len, batch_idx); */
          x.score = score;
          batch->xs[n++] = x;
        }
      }
    }
    batch->len = n;
    if (invalid_candidate)
      break;
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

/* Dispatch fzf positions on CSTR to HOOK as offsets in STR's representation.
   POS->data[] is fzf's descending character-offset list; consolidated
   into ascending contiguous runs, mapped back to byte offsets when STR is
   unibyte, packed into
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

  /* The matcher emits logical character offsets.  Emacs multibyte strings use
     the same coordinate system.  Lossless raw-byte candidates, however, are
     represented as unibyte strings even when a valid UTF-8 prefix decoded to
     fewer matcher characters.  Map the logical runs back to byte indices for
     those strings so a match after mixed valid/invalid input is highlighted at
     the byte Emacs actually exposes. */
  emacs_value is_multibyte =
      env->funcall(env, Fmultibyte_string_p, 1, &str);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    env->non_local_exit_clear(env);
    if (need_free) { free(starts); free(ends); free(vargs); }
    return;
  }
  if (env->eq(env, is_multibyte, Qnil)) {
    size_t byte_len = strlen(cstr);
    for (size_t i = 0; i < n_runs; ++i) {
      starts[i] = utf8_prefix_byte_length(cstr, byte_len, starts[i]);
      ends[i] = utf8_prefix_byte_length(cstr, byte_len, ends[i]);
    }
  }

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
static bool apply_highlight_positions(emacs_env *env,
                                      const char *cstr,
                                      fzf_pattern_t *pattern,
                                      fzf_slab_t *slab,
                                      emacs_value str_val,
                                      emacs_value hook,
                                      HlScratch *scratch) {
  if (cstr[0] == '\0') return true;
  if (env->eq(env, hook, Qnil)) return true;
  fzf_position_t *pos = fzf_get_positions(cstr, pattern, slab);
  if (fzf_allocation_failed()) {
    fzf_free_positions(pos);
    async_signal_error(
        env, "fzf-native: matcher could not allocate highlight positions");
    return false;
  }
  dispatch_highlight_runs(env, cstr, pos, str_val, hook, scratch);
  fzf_free_positions(pos);
  return true;
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
  if (reject_embedded_nul(env, query, "query")) goto err;

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
  if (!pattern) {
    async_signal_error(
        env, "fzf-native: matcher could not allocate parsed query");
    goto err;
  }
  struct Shared shared = {
    .pattern = pattern,
    .batches = batches,
    .remaining = batch_idx + 1,
    .filter_only = filter_only_mode,
    .allocation_failed = false,
    .embedded_nul = false,
  };

#ifdef _WIN32
  worker_routine(&shared);
#endif
#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
  // Print the shared value.
  /* ssize_t value = atomic_load(&shared.remaining); */
  /* printf("shared Remaining: %zd\n", value); */
  // Set up a bounded worker count from the fallible processor probe.
  unsigned max_workers = fzf_worker_count(
      sysconf(_SC_NPROCESSORS_ONLN), batch_idx + 1);

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
  if (shared_embedded_nul(&shared)) {
    success = false;
    async_signal_error(
        env, "fzf-native: embedded NUL in candidate is not supported");
  } else if (shared_allocation_failed(&shared)) {
    success = false;
    async_signal_error(
        env, "fzf-native: matcher could not allocate scoring scratch");
  }
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
      if (!apply_highlight_positions(env, xs[i].s.b, hl_pattern, hl_slab,
                                     out_val, hl_hook, &hl_scratch))
        break;
    }

    emacs_value cell =
        env->funcall(env, Fcons, 2, (emacs_value[]) { out_val, result });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) break;
    result = cell;
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
  if (reject_embedded_nul(env, query, "query")) goto done;
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
    if (!pattern) {
      async_signal_error(
          env, "fzf-native: matcher could not allocate parsed query");
      goto done;
    }
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
        if (reject_embedded_nul(env, s, "candidate")) goto done;
        if (s.b) {
          emacs_value cp = try_copy_string(env, value);
          if (!apply_highlight_positions(env, s.b, pattern, slab, cp, hook,
                                         &hl_scratch))
            goto done;
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
        if (reject_embedded_nul(env, s, "candidate")) goto done;
        if (s.b) {
          emacs_value cp = try_copy_string(env, value);
          if (!apply_highlight_positions(env, s.b, pattern, slab, cp, hook,
                                         &hl_scratch))
            goto done;
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
  emacs_value cp = Qnil;

  struct Str query = copy_emacs_string(env, &bump, args[1]);
  if (reject_embedded_nul(env, query, "query")) goto done;
  bool clear_only = (!query.b || query.len == 0);

  /* Fresh copy of CAND so caller's literal is never face-mutated.  Even
     the clear-only path acts on the copy, not the original. */
  cp = try_copy_string(env, args[0]);

  if (clear_only) {
    clear_highlight_face(env, cp);
    goto done;
  }

  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  bool           fuzzy     = resolve_fzf_native_fuzzy(env);
  pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
  if (!pattern) {
    async_signal_error(
        env, "fzf-native: matcher could not allocate parsed query");
    goto done;
  }
  slab = fzf_make_default_slab();
  if (!slab) goto done;
  emacs_value hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
  hl_scratch_init(&hl_scratch, query.len);

  struct Str s = copy_emacs_string(env, &bump, args[0]);
  if (reject_embedded_nul(env, s, "candidate")) goto done;
  if (s.b) {
    (void)apply_highlight_positions(
        env, s.b, pattern, slab, cp, hook, &hl_scratch);
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
  fzf_pattern_t *pattern = NULL;
  fzf_slab_t *slab = NULL;
  bool own_slab = false;

  struct Str str = copy_emacs_string(env, &bump, args[0]);
  if (!str.b) { goto err; }
  if (reject_embedded_nul(env, str, "candidate")) goto err;

  struct Str query = copy_emacs_string(env, &bump, args[1]);
  if (!query.b) { goto err; }
  if (reject_embedded_nul(env, query, "query")) goto err;

  fzf_log("fzf_native_score: str='%.*s' query='%.*s'\n", (int)str.len, str.b, (int)query.len, query.b);

  /* fzf_case_mode enum : CaseSmart = 0, CaseIgnore, CaseRespect
   * normalize bool     : Always set to false because its not implemented yet.
   *                      This is reserved for future use
   * pattern char*      : Pattern you want to match. e.g. "src | lua !.c$
   * fuzzy bool         : Enable or disable fuzzy matching
   */
  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  bool           fuzzy     = resolve_fzf_native_fuzzy(env);
  pattern = fzf_parse_pattern(case_mode, false, query.b, fuzzy);
  if (!pattern) {
    async_signal_error(
        env, "fzf-native: matcher could not allocate parsed query");
    goto err;
  }

  if (nargs > 2) {
    // Re-use SLAB argument.
    slab = fzf_native_typed_user_ptr(
        env, args[2], slab_finalize, "an fzf slab handle");
    if (!slab) goto err;
  } else {
    // Create a one-time use slab.
    slab = fzf_make_default_slab();
    own_slab = true;
    if (!slab) {
      async_signal_error(
          env, "fzf-native: matcher could not allocate scoring slab");
      goto err;
    }
  }

  int score = fzf_get_score(str.b, pattern, slab);
  if (fzf_allocation_failed()) {
    async_signal_error(env, "fzf-native: matcher could not allocate scoring scratch");
    goto err;
  }

  /* Apply C-layer highlighting when fussy-fzf-native-highlight is non-nil
     and the candidate matched.  The cap concept does not apply to a single
     candidate — any non-nil value enables highlighting for this call. */
  if (score > 0 && resolve_fussy_highlight_cap(env, 1) > 0) {
    emacs_value hook = defcustom_value(env, Qsym_highlight_fn, Qnil);
    if (!apply_highlight_positions(
            env, str.b, pattern, slab, args[0], hook, NULL))
      goto err;
  }

  /* Return (SCORE) — a single-element list.  Match indices are no longer
     surfaced to Elisp; highlighting is handled in C. */
  emacs_value score_val = env->make_integer(env, score);
  result = env->funcall(env, Flist, 1, &score_val);

err:
  if (pattern) fzf_free_pattern(pattern);
  if (own_slab && slab) fzf_free_slab(slab);
  bump_free(bump);
  /* On coercion failure we return Qlistofzero (no match) rather than
     signaling, so a single un-coerceable input doesn't blow up a
     larger completion batch. Empty QUERY short-circuits to the same
     value above; empty STR must reach the matcher because an inverse
     term can legitimately match it. */
  return result;
}

static void slab_finalize(void *object) {
  fzf_slab_t *slab = (fzf_slab_t *)object;
  fzf_free_slab(slab);
}

static emacs_value signal_slab_allocation_error(emacs_env *env) {
  static const char message[] = "fzf-native: invalid or unavailable slab size";
  emacs_value string =
      env->make_string(env, message, (ptrdiff_t)(sizeof(message) - 1));
  emacs_value data = env->funcall(env, Flist, 1, &string);
  env->non_local_exit_signal(env, Qerror, data);
  return Qnil;
}

/* make_user_ptr allocates both a Lisp_User_Ptr and an emacs_value slot.  If
   the second allocation fails, the Lisp object can still be finalized even
   though C receives no handle.  Publish a NULL payload first so every failure
   is inert, then attach native ownership with non-allocating set_user_ptr. */
static bool fzf_native_make_inert_user_ptr(emacs_env *env,
                                           emacs_finalizer finalizer,
                                           emacs_value *handle) {
  *handle = env->make_user_ptr(env, finalizer, NULL);
  return *handle != NULL &&
         env->non_local_exit_check(env) == emacs_funcall_exit_return;
}

emacs_value fzf_native_make_default_slab(emacs_env *env,
                                         ptrdiff_t UNUSED(nargs),
                                         emacs_value UNUSED(args[]),
                                         void UNUSED(*data_ptr)) {
  emacs_value handle;
  if (!fzf_native_make_inert_user_ptr(env, slab_finalize, &handle))
    return Qnil;
  fzf_slab_t *slab = fzf_make_default_slab();

  if (!slab)
    return signal_slab_allocation_error(env);

  env->set_user_ptr(env, handle, slab);
  return handle;
}

emacs_value fzf_native_make_slab(emacs_env *env,
                                 ptrdiff_t UNUSED(nargs),
                                 emacs_value args[],
                                 void UNUSED(*data_ptr)) {
  intmax_t slab16 = env->extract_integer(env, args[0]);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return Qnil;
  intmax_t slab32 = env->extract_integer(env, args[1]);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return Qnil;
  if (slab16 < 0 || slab32 < 0 ||
      (uintmax_t)slab16 > SIZE_MAX || (uintmax_t)slab32 > SIZE_MAX)
    return signal_slab_allocation_error(env);

  emacs_value handle;
  if (!fzf_native_make_inert_user_ptr(env, slab_finalize, &handle))
    return Qnil;

  fzf_slab_t *slab = fzf_make_slab(
      (fzf_slab_config_t){(size_t)slab16, (size_t)slab32});
  if (!slab)
    return signal_slab_allocation_error(env);

  env->set_user_ptr(env, handle, slab);
  return handle;
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

static bool async_stop_requested(_Atomic bool *stop) {
  return stop && atomic_load_explicit(stop, memory_order_relaxed);
}

static bool async_copy_bytes_abortable(void *destination, const void *source,
                                       size_t bytes, _Atomic bool *stop) {
  const size_t chunk = 64 * 1024;
  unsigned char *dst = destination;
  const unsigned char *src = source;
  for (size_t offset = 0; offset < bytes; offset += chunk) {
    if (stop && atomic_load_explicit(stop, memory_order_relaxed)) return false;
    size_t amount = MIN(chunk, bytes - offset);
    memcpy(dst + offset, src + offset, amount);
  }
  return !(stop && atomic_load_explicit(stop, memory_order_relaxed));
}

static SharedIdx *shared_idx_alloc_abortable(const uint32_t *src, size_t n,
                                              _Atomic bool *stop) {
  if ((n && !src) || n > (SIZE_MAX - sizeof(SharedIdx)) / sizeof *src)
    return NULL;
  SharedIdx *p = malloc(sizeof *p + n * sizeof *p->idx);
  if (!p) return NULL;
  atomic_init(&p->refcount, 1);
  p->count = n;
  if (n && !async_copy_bytes_abortable(
               p->idx, src, n * sizeof *p->idx, stop)) {
    free(p);
    return NULL;
  }
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
  size_t          bytes;
} CacheEntry;

typedef struct {
  pthread_mutex_t mu;
  CacheEntry     *head;     /* MRU */
  CacheEntry     *tail;     /* LRU */
  size_t          count;
  size_t          max_entries;
  size_t          used_bytes;
  size_t          max_bytes;
  uint64_t        evictions;
} Cache;

static void cache_init_limits(Cache *c, size_t max_entries,
                              size_t max_bytes) {
  pthread_mutex_init(&c->mu, NULL);
  c->head = c->tail = NULL;
  c->count = 0;
  c->max_entries = max_entries ? max_entries : 40;
  c->used_bytes = 0;
  c->max_bytes = max_bytes;
  c->evictions = 0;
}

static void cache_init(Cache *c, size_t max_entries) {
  cache_init_limits(c, max_entries, SIZE_MAX);
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
  c->used_bytes -= e->bytes;
}

static void cache_push_head_locked(Cache *c, CacheEntry *e) {
  e->prev = NULL;
  e->next = c->head;
  if (c->head) c->head->prev = e;
  c->head = e;
  if (!c->tail) c->tail = e;
  c->count++;
  c->used_bytes += e->bytes;
}

static void cache_free(Cache *c) {
  pthread_mutex_lock(&c->mu);
  CacheEntry *e = c->head;
  while (e) { CacheEntry *n = e->next; cache_entry_free(e); e = n; }
  c->head = c->tail = NULL;
  c->count = 0;
  c->used_bytes = 0;
  pthread_mutex_unlock(&c->mu);
  pthread_mutex_destroy(&c->mu);
}

static bool cache_bytes_add(size_t *total, size_t amount) {
  if (amount > SIZE_MAX - *total) return false;
  *total += amount;
  return true;
}

static bool cache_bytes_mul_add(size_t *total, size_t count,
                                size_t element_size) {
  if (count && element_size > (SIZE_MAX - *total) / count) return false;
  *total += count * element_size;
  return true;
}

static size_t cache_pattern_bytes(const fzf_pattern_t *pattern) {
  if (!pattern) return 0;
  size_t bytes = 0;
  if (!cache_bytes_add(&bytes, sizeof *pattern) ||
      !cache_bytes_mul_add(
          &bytes, pattern->cap, sizeof *pattern->ptr))
    return SIZE_MAX;
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    if (!cache_bytes_add(&bytes, sizeof *set) ||
        !cache_bytes_mul_add(&bytes, set->cap, sizeof *set->ptr))
      return SIZE_MAX;
    for (size_t j = 0; j < set->size; j++) {
      size_t text_len = strlen(set->ptr[j].ptr);
      const fzf_string_t *text = set->ptr[j].text;
      if (!cache_bytes_add(&bytes, sizeof(fzf_string_t)) ||
          !cache_bytes_add(&bytes, text_len) ||
          !cache_bytes_add(&bytes, 1) ||
          (text && !cache_bytes_mul_add(
                       &bytes, text->codepoint_count,
                       sizeof *text->codepoints)))
        return SIZE_MAX;
    }
  }
  return bytes;
}

static size_t cache_entry_bytes(const char *query, size_t top_count,
                                const SharedIdx *m_idx,
                                const fzf_pattern_t *parsed) {
  size_t bytes = 0;
  size_t query_len = strlen(query);
  if (!cache_bytes_add(&bytes, sizeof(CacheEntry)) ||
      !cache_bytes_add(&bytes, query_len) ||
      !cache_bytes_add(&bytes, 1) ||
      !cache_bytes_mul_add(&bytes, top_count, sizeof(ScoredStr)))
    return SIZE_MAX;
  if (m_idx) {
    if (!cache_bytes_add(&bytes, sizeof *m_idx) ||
        !cache_bytes_mul_add(
            &bytes, m_idx->count, sizeof *m_idx->idx))
      return SIZE_MAX;
  }
  size_t parsed_bytes = cache_pattern_bytes(parsed);
  if (!cache_bytes_add(&bytes, parsed_bytes)) return SIZE_MAX;
  return bytes;
}

static void cache_stats(Cache *c, size_t *entries, size_t *bytes,
                        uint64_t *evictions) {
  pthread_mutex_lock(&c->mu);
  *entries = c->count;
  *bytes = c->used_bytes;
  *evictions = c->evictions;
  pthread_mutex_unlock(&c->mu);
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

/* A full-scoring cached top-K covers LIMIT when it contains every match, or
   when it holds at least the requested positive capacity.  LIMIT 0 requests
   every match.  Filter-only results use a stricter rule below because their
   ranked window is selected in pool order before it is scored. */
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

  size_t copy_count = e->top_count;
  if (limit > 0 && copy_count > limit) copy_count = limit;
  ScoredStr *top_copy = NULL;
  if (copy_count) {
    top_copy = malloc(copy_count * sizeof *top_copy);
    if (top_copy) memcpy(top_copy, e->top, copy_count * sizeof *top_copy);
  }
  *out_top       = top_copy;
  *out_top_count = top_copy ? copy_count : 0;
  *out_m_idx     = shared_idx_retain(e->m_idx);
  *out_pool_gen  = e->pool_gen;
  *out_matched_count = e->matched_count;
  bool top_copy_complete = copy_count == 0 || top_copy != NULL;
  size_t requested_count =
      limit && limit < e->matched_count ? limit : e->matched_count;
  /* Filter-only mode matches the whole pool cheaply, then scores only the
     first requested_count matches in pool order.  A cached larger window
     cannot be truncated after ranking: doing so can select a candidate that
     was outside the smaller request's input-order window.  Exact reuse is
     therefore safe only when both requests selected the same-sized window.
     The exhaustive membership set remains reusable when this test fails. */
  bool capacity_covered = filter_only
                              ? e->top_count == requested_count
                              : cache_entry_covers_limit(e, limit);
  *out_result_covered = top_copy_complete &&
                        e->filter_only == filter_only &&
                        capacity_covered;

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
  if (!e->m_idx) {
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
static void cache_insert_for_request_abortable(
    Cache *c, const char *query, size_t pool_gen,
    fzf_case_types case_mode, bool fuzzy, bool filter_only,
    const ScoredStr *top, size_t top_count, size_t matched_count,
    const uint32_t *m_idx_src, size_t m_idx_count,
    _Atomic bool *stop) {
  if (c->max_bytes == 0) return;
  /* Pre-allocate everything outside the mutex. */
  char *q_dup = strdup(query);
  ScoredStr *top_dup = NULL;
  if (top_count && top) {
    top_dup = malloc(top_count * sizeof *top_dup);
    if (top_dup) {
      if (!async_copy_bytes_abortable(
              top_dup, top, top_count * sizeof *top_dup, stop)) {
        free(top_dup);
        top_dup = NULL;
        top_count = 0;
      }
    } else {
      top_count = 0;
    }
  }
  bool membership_complete =
      matched_count == 0 ||
      (m_idx_src && m_idx_count == matched_count);
  SharedIdx *sidx = !strchr(query, '|') && membership_complete
                    ? shared_idx_alloc_abortable(
                          m_idx_src, m_idx_count, stop) : NULL;
  /* Parse once on insert so cache_lookup_prefix doesn't pay parse cost on
     every iteration of its scan loop.  NULL is fine — entries with NULL
     parsed only participate via the byte-prefix subsumption fallback. */
  fzf_pattern_t *parsed = parse_query_for_cache(query, case_mode, fuzzy);

  if (!q_dup || async_stop_requested(stop)) {
    free(q_dup);
    free(top_dup);
    shared_idx_release(sidx);
    if (parsed) fzf_free_pattern(parsed);
    return;
  }

  size_t stored_top_count = top_dup ? top_count : 0;
  size_t entry_bytes = cache_entry_bytes(q_dup, stored_top_count,
                                         sidx, parsed);

  pthread_mutex_lock(&c->mu);
  if (c->max_bytes == 0 || entry_bytes > c->max_bytes) {
    pthread_mutex_unlock(&c->mu);
    free(q_dup);
    free(top_dup);
    shared_idx_release(sidx);
    if (parsed) fzf_free_pattern(parsed);
    return;
  }
  CacheEntry *e = cache_find_locked(c, query, case_mode, fuzzy);
  if (e) {
    /* Update existing entry: swap fields, release old refs after unlock. */
    char *old_q = e->query;
    ScoredStr *old_top = e->top;
    SharedIdx *old_idx = e->m_idx;
    fzf_pattern_t *old_parsed = e->parsed;
    cache_unlink_locked(c, e);
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
    e->bytes     = entry_bytes;
    cache_push_head_locked(c, e);
    CacheEntry *evicted = NULL;
    while ((c->count > c->max_entries || c->used_bytes > c->max_bytes) &&
           c->tail) {
      CacheEntry *victim = c->tail;
      cache_unlink_locked(c, victim);
      victim->next = evicted;
      evicted = victim;
      c->evictions++;
    }
    pthread_mutex_unlock(&c->mu);
    free(old_q);
    free(old_top);
    shared_idx_release(old_idx);
    if (old_parsed) fzf_free_pattern(old_parsed);
    while (evicted) {
      CacheEntry *next = evicted->next;
      cache_entry_free(evicted);
      evicted = next;
    }
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
  ne->bytes     = entry_bytes;
  cache_push_head_locked(c, ne);

  /* Evict LRU entries until both count and byte limits are satisfied. */
  CacheEntry *evicted = NULL;
  while ((c->count > c->max_entries || c->used_bytes > c->max_bytes) &&
         c->tail) {
    CacheEntry *victim = c->tail;
    cache_unlink_locked(c, victim);
    victim->next = evicted;
    evicted = victim;
    c->evictions++;
  }
  pthread_mutex_unlock(&c->mu);
  while (evicted) {
    CacheEntry *next = evicted->next;
    cache_entry_free(evicted);
    evicted = next;
  }
}

static void cache_insert_for_request(
    Cache *c, const char *query, size_t pool_gen,
    fzf_case_types case_mode, bool fuzzy, bool filter_only,
    const ScoredStr *top, size_t top_count, size_t matched_count,
    const uint32_t *m_idx_src, size_t m_idx_count) {
  cache_insert_for_request_abortable(
      c, query, pool_gen, case_mode, fuzzy, filter_only,
      top, top_count, matched_count, m_idx_src, m_idx_count, NULL);
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
  size_t           entry_count;
  size_t           used_bytes;
  size_t           max_bytes;
  uint64_t         hits;
  uint64_t         misses;
  uint64_t         inserts;
  uint64_t         evictions;
} BatchCache;

static void batch_cache_remove_entry_locked(
    BatchCache *c, BatchCacheEntry *entry, bool eviction);

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
  size_t query_len = strlen(query_copy);
  size_t parsed_bytes = cache_pattern_bytes(parsed);
  created->bytes = 0;
  if (!cache_bytes_add(&created->bytes, sizeof *created) ||
      !cache_bytes_add(&created->bytes, query_len) ||
      !cache_bytes_add(&created->bytes, 1) ||
      !cache_bytes_add(&created->bytes, parsed_bytes)) {
    free(created->query);
    if (created->parsed) fzf_free_pattern(created->parsed);
    free(created);
    return NULL;
  }

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
  while (c->tail &&
         (c->used_bytes > c->max_bytes ||
          created->bytes > c->max_bytes - c->used_bytes))
    batch_cache_remove_entry_locked(c, c->tail, true);
  if (c->used_bytes > c->max_bytes ||
      created->bytes > c->max_bytes - c->used_bytes) {
    pthread_mutex_unlock(&c->mu);
    free(created->query);
    if (created->parsed) fzf_free_pattern(created->parsed);
    free(created);
    return NULL;
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
  c->entry_count--;
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

  while (c->tail &&
         (c->used_bytes > c->max_bytes ||
          created->bytes > c->max_bytes - c->used_bytes))
    batch_cache_remove_entry_locked(c, c->tail, true);
  if (c->used_bytes > c->max_bytes ||
      created->bytes > c->max_bytes - c->used_bytes) {
    pthread_mutex_unlock(&c->mu);
    batch_cache_entry_free(created);
    return;
  }

  size_t bucket = batch_cache_hash(c, query, batch_id);
  created->hash_next = c->buckets[bucket];
  c->buckets[bucket] = created;
  batch_cache_lru_push_locked(c, created);
  query->entry_count++;
  c->entry_count++;
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
  *entries = c->entry_count;
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
  c->entry_count = 0;
  pthread_mutex_unlock(&c->mu);
  pthread_mutex_destroy(&c->mu);
}

struct AsyncWorkerPool;
static struct AsyncWorkerPool *async_worker_pool_create(unsigned count);
static void async_worker_pool_destroy(struct AsyncWorkerPool *pool);
static void async_worker_pool_wake(struct AsyncWorkerPool *pool);

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
  AsyncProducerErrorInvalidData,
  AsyncProducerErrorAllocation,
  AsyncProducerErrorCapacity,
  AsyncProducerErrorWait,
};

typedef struct {
  pthread_t     reader;
  bool          reader_started;
  pid_t         pid;
  /* CHILD_OWNER selects the one thread allowed to reap PID.  It does not
     grant exclusive signalling rights: teardown must still be able to stop
     a producer after the reader has observed EOF and entered waitpid.
     CHILD_ALIVE is protected by CHILD_MU so signalling cannot race a reap
     and accidentally target a reused pid. */
  _Atomic int   child_owner;
  pthread_mutex_t child_mu;
  bool          child_alive;
  _Atomic int   producer_state;
  /* Error kind and number are one atomic publication.  Publishing them in
     separate atomics allowed readers to observe a new kind with the old
     (usually zero) errno/status. */
  _Atomic uint64_t producer_error;
  _Atomic int   producer_exit_status;
  FILE         *fp;
  int           cancel_read_fd;
  int           cancel_write_fd;
  bool          cancel_pipe_ready;
#ifdef FZF_NATIVE_CTEST
  /* Deterministic coverage seam: native tests and the session fuzzer wait for
     the reader to enter a specific poll epoch before publishing stop. */
  _Atomic unsigned test_reader_poll_epoch;
#endif
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
  _Atomic(struct AsyncWorkerPool *) worker_pool;
  bool             worker_pool_owned;
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
     `fzf_has_match' (see fzf-additions.c), retains the first limit
     matches in pool order, then ranks that bounded window.  Complete
     membership is retained only when it fits its independent cache cap;
     otherwise later refinement safely uses other evidence or rescans.
     See `fzf-native-filter-only-min-pool'. */
  size_t           filter_only_min_pool;
} AsyncSession;

typedef struct {
  size_t count;
  bool reader_done;
} AsyncPoolObservation;

typedef struct {
  uint64_t request_id;
  size_t pool_generation;
} AsyncResultObservation;

typedef struct {
  AsyncResultObservation result;
  AsyncPoolObservation pool;
  bool stale;
} AsyncSnapshotState;

#ifdef FZF_NATIVE_CTEST
/* Deterministic seam for the pool-observation ordering regression.  The test
   wakes a simulated reader after COUNT is sampled but before READER_DONE is
   loaded; production builds have no branch or storage for this hook. */
static void (*async_test_pool_observation_hook)(AsyncSession *s);
#endif

/* The reader appends under MU and publishes READER_DONE only after its final
   append.  Observe both while holding MU so no caller can pair an old count
   with a later true terminal marker.  Seeing the new count with a false
   marker is conservative and valid during terminal publication. */
static AsyncPoolObservation async_observe_pool(AsyncSession *s) {
  pthread_mutex_lock(&s->mu);
  AsyncPoolObservation observation = {
      .count = s->count,
  };
#ifdef FZF_NATIVE_CTEST
  if (async_test_pool_observation_hook)
    async_test_pool_observation_hook(s);
#endif
  observation.reader_done = atomic_load_explicit(
      &s->reader_done, memory_order_acquire);
  pthread_mutex_unlock(&s->mu);
  return observation;
}

static int async_normalize_wait_status(int status) {
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
  return -1;
}

static uint64_t async_pack_producer_error(
    enum AsyncProducerErrorKind kind, int error_number) {
  return ((uint64_t)(uint32_t)kind << 32) | (uint32_t)error_number;
}

static enum AsyncProducerErrorKind async_unpack_producer_error_kind(
    uint64_t error) {
  return (enum AsyncProducerErrorKind)(uint32_t)(error >> 32);
}

static int async_unpack_producer_error_number(uint64_t error) {
  return (int)(uint32_t)error;
}

static void async_record_producer_failure(AsyncSession *s,
                                          enum AsyncProducerErrorKind kind,
                                          int error_number) {
  uint64_t expected = 0;
  atomic_compare_exchange_strong_explicit(
      &s->producer_error, &expected,
      async_pack_producer_error(kind, error_number),
      memory_order_release, memory_order_relaxed);
}

static bool async_claim_child(AsyncSession *s, enum AsyncChildOwner owner) {
  int expected = AsyncChildUnclaimed;
  return atomic_compare_exchange_strong_explicit(
      &s->child_owner, &expected, owner,
      memory_order_acq_rel, memory_order_acquire);
}

/* Signal the producer process group while PID is known to be live.  The
   shell is made a process-group leader at start so descendants that retain
   stdout cannot keep the reader blocked after stop.  If process-group setup
   lost a platform race, fall back to the direct child. */
static bool async_signal_child(AsyncSession *s, int signal_number) {
  bool signalled = false;
  pthread_mutex_lock(&s->child_mu);
  if (s->child_alive && s->pid > 0) {
    if (kill(-s->pid, signal_number) == 0) {
      signalled = true;
    } else if (errno == ESRCH && kill(s->pid, signal_number) == 0) {
      signalled = true;
    }
  }
  pthread_mutex_unlock(&s->child_mu);
  return signalled;
}

/* A producer that has completed exec may install signal handlers immediately.
   Startup failures therefore cannot rely on SIGTERM before a blocking reap.
   Kill the whole process group, fall back to the direct child if group setup
   raced, and retry waitpid only across EINTR. */
static void async_kill_and_reap_spawned_child(pid_t pid) {
  if (pid <= 0) return;
  if (kill(-pid, SIGKILL) != 0)
    (void)kill(pid, SIGKILL);
  while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {}
}

/* Reap without holding CHILD_MU across a blocking wait.  This leaves a
   signalling window for teardown when the reader owns the child after EOF
   but the producer remains alive. */
static pid_t async_wait_child(AsyncSession *s, int *wait_status) {
  for (;;) {
    int saved_errno = 0;
    pthread_mutex_lock(&s->child_mu);
    if (!s->child_alive || s->pid <= 0) {
      pthread_mutex_unlock(&s->child_mu);
      errno = ECHILD;
      return -1;
    }
    pid_t waited = waitpid(s->pid, wait_status, WNOHANG);
    saved_errno = errno;
    if (waited == s->pid || (waited < 0 && saved_errno == ECHILD))
      s->child_alive = false;
    pthread_mutex_unlock(&s->child_mu);

    if (waited == s->pid) return waited;
    if (waited < 0 && saved_errno != EINTR) {
      errno = saved_errno;
      return waited;
    }
    if (waited == 0) {
      struct timespec delay = {.tv_sec = 0, .tv_nsec = 5 * 1000 * 1000};
      while (nanosleep(&delay, &delay) != 0 && errno == EINTR) {}
    }
  }
}

static void async_publish_producer_terminal(AsyncSession *s) {
  atomic_store_explicit(&s->reader_done, true, memory_order_release);
  pthread_mutex_lock(&s->score_res_mu);
  s->score_snapshot_generation++;
  pthread_mutex_unlock(&s->score_res_mu);
  atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
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

/* A producer descendant can leave the producer's process group with setsid()
   while retaining stdout.  Process signalling is therefore best-effort
   cleanup, not a reader wakeup mechanism.  Give every session an independent
   close-on-exec cancellation channel that teardown owns. */
static bool async_make_cancel_pipe(AsyncSession *s) {
  int fds[2];
  if (pipe(fds) != 0) return false;

  int read_fd_flags = fcntl(fds[0], F_GETFD);
  int write_fd_flags = fcntl(fds[1], F_GETFD);
  int write_status_flags = fcntl(fds[1], F_GETFL);
  if (read_fd_flags < 0 || write_fd_flags < 0 || write_status_flags < 0 ||
      fcntl(fds[0], F_SETFD, read_fd_flags | FD_CLOEXEC) < 0 ||
      fcntl(fds[1], F_SETFD, write_fd_flags | FD_CLOEXEC) < 0 ||
      fcntl(fds[1], F_SETFL, write_status_flags | O_NONBLOCK) < 0) {
    int saved_errno = errno;
    close(fds[0]);
    close(fds[1]);
    errno = saved_errno;
    return false;
  }

  s->cancel_read_fd = fds[0];
  s->cancel_write_fd = fds[1];
  s->cancel_pipe_ready = true;
  return true;
}

static void async_wake_reader(AsyncSession *s) {
  if (!s->cancel_pipe_ready) return;
  const unsigned char wake = 1;
  ssize_t written;
  do {
    written = write(s->cancel_write_fd, &wake, sizeof wake);
  } while (written < 0 && errno == EINTR);
  /* EAGAIN means an earlier wake byte is already pending. */
}

/* Serialize the stop publication with candidate publication under MU.  A
   candidate that acquires MU first is ordered before stop; one that acquires
   it afterwards observes STOP and cannot become visible. */
static void async_publish_stop(AsyncSession *s) {
  pthread_mutex_lock(&s->mu);
  atomic_store_explicit(&s->stop, true, memory_order_release);
  pthread_mutex_unlock(&s->mu);
  async_wake_reader(s);
}

/* Append one immutable candidate to the session arena and publish it to the
   scorer.  The producer is the sole caller, so reading COUNT and the current
   top-level block pointer before taking MU is safe.  Keeping this operation
   separate from getline lets the native interactive fuzzer drive the real
   growth path without an Emacs process or a shell child. */
static bool async_append_candidate(AsyncSession *s, const char *line,
                                   size_t len) {
  if (atomic_load_explicit(&s->stop, memory_order_acquire)) return false;
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
  if (atomic_load_explicit(&s->stop, memory_order_acquire)) {
    pthread_mutex_unlock(&s->mu);
    if (need_publish) free(block);
    return false;
  }
  if (need_publish) s->cands_top[hi] = block;
  s->cands_top[hi][lo] = dup;
  s->count++;
  pthread_mutex_unlock(&s->mu);
  atomic_fetch_add_explicit(&s->gen, 1, memory_order_relaxed);
  async_notify_candidate_growth(s);
  return true;
}

static bool async_extend_line(AsyncSession *s, char **line, size_t *used,
                              size_t *capacity, const char *bytes,
                              size_t amount) {
  if (amount > SIZE_MAX - *used - 1) {
    async_record_producer_failure(
        s, AsyncProducerErrorAllocation, ENOMEM);
    return false;
  }
  size_t needed = *used + amount + 1;
  if (needed > *capacity) {
    size_t next = *capacity ? *capacity : 4096;
    while (next < needed) {
      if (next > SIZE_MAX / 2) {
        next = needed;
        break;
      }
      next *= 2;
    }
    char *grown = realloc(*line, next);
    if (!grown) {
      async_record_producer_failure(
          s, AsyncProducerErrorAllocation, ENOMEM);
      return false;
    }
    *line = grown;
    *capacity = next;
  }
  if (amount) memcpy(*line + *used, bytes, amount);
  *used += amount;
  (*line)[*used] = '\0';
  return true;
}

static bool async_process_candidate_line(AsyncSession *s, char *line,
                                         size_t len) {
  /* Every matcher and snapshot consumer currently uses NUL-terminated
     candidate strings.  Reject an embedded NUL at the reader boundary so a
     producer can never publish a silently shortened candidate. */
  if (len && memchr(line, '\0', len) != NULL) {
    async_record_producer_failure(s, AsyncProducerErrorInvalidData, 0);
    return false;
  }

  while (len && line[len - 1] == '\r') line[--len] = '\0';
  len = async_strip_ansi(line, len);

  ptrdiff_t mll = s->max_line_length;
  if (mll != 0) {
    size_t cap = mll > 0
                     ? (size_t)mll
                     : (size_t)(-(mll + 1)) + 1;
    size_t char_len = utf8_character_count(line, len);
    if (char_len > cap) {
      if (mll > 0) return true;   /* exclude */
      len = utf8_prefix_byte_length(line, len, cap); /* truncate */
      line[len] = '\0';
    }
  }

  return async_append_candidate(s, line, len);
}

static void *async_reader(void *arg) {
  fzf_block_all_signals();
  AsyncSession *s = arg;
  fzf_log("async_reader START: pid=%d\n", (int)s->pid);

  /* Use fd-level poll/read instead of getline.  getline can block forever on
     an unterminated line held open by a descendant outside the producer's
     process group.  The cancellation pipe gives teardown an owned wakeup;
     raw reads retain getline's unbounded-line and final-unterminated-line
     semantics without ever blocking while a partial line is buffered. */
  char *line = NULL;
  size_t used = 0;
  size_t capacity = 0;
  bool eof = false;
  bool read_failed = false;
  int output_fd = s->fp ? fileno(s->fp) : -1;

  if (output_fd < 0) {
    async_record_producer_failure(
        s, AsyncProducerErrorRead, errno ? errno : EBADF);
    read_failed = true;
  }

  while (!read_failed &&
         !atomic_load_explicit(&s->stop, memory_order_acquire)) {
    struct pollfd wait_fds[2] = {
      {.fd = output_fd, .events = POLLIN},
      {.fd = s->cancel_read_fd, .events = POLLIN},
    };
    nfds_t wait_count = s->cancel_pipe_ready ? 2 : 1;
    int ready;
#ifdef FZF_NATIVE_CTEST
    atomic_fetch_add_explicit(
        &s->test_reader_poll_epoch, 1, memory_order_release);
#endif
    do {
      ready = poll(wait_fds, wait_count, -1);
    } while (ready < 0 && errno == EINTR &&
             !atomic_load_explicit(&s->stop, memory_order_acquire));

    /* Cancellation wins when producer data and the wake byte arrive in the
       same poll cycle. */
    if (atomic_load_explicit(&s->stop, memory_order_acquire) ||
        (s->cancel_pipe_ready &&
         (wait_fds[1].revents & (POLLIN | POLLHUP | POLLERR | POLLNVAL))))
      break;
    if (ready < 0) {
      async_record_producer_failure(
          s, AsyncProducerErrorRead, errno ? errno : EIO);
      read_failed = true;
      break;
    }
    if (wait_fds[0].revents & POLLNVAL) {
      async_record_producer_failure(s, AsyncProducerErrorRead, EBADF);
      read_failed = true;
      break;
    }
    if (!(wait_fds[0].revents & (POLLIN | POLLHUP | POLLERR))) continue;

    char chunk[8192];
    ssize_t amount;
    do {
      amount = read(output_fd, chunk, sizeof chunk);
    } while (amount < 0 && errno == EINTR &&
             !atomic_load_explicit(&s->stop, memory_order_acquire));
    if (atomic_load_explicit(&s->stop, memory_order_acquire)) break;
    if (amount < 0) {
      async_record_producer_failure(
          s, AsyncProducerErrorRead, errno ? errno : EIO);
      read_failed = true;
      break;
    }
    if (amount == 0) {
      eof = true;
      break;
    }

    size_t segment = 0;
    bool keep_reading = true;
    for (size_t i = 0; i < (size_t)amount; i++) {
      if (chunk[i] != '\n') continue;
      if (!async_extend_line(
              s, &line, &used, &capacity, chunk + segment, i - segment) ||
          !async_process_candidate_line(s, line, used)) {
        keep_reading = false;
        break;
      }
      used = 0;
      line[0] = '\0';
      segment = i + 1;
    }
    if (!keep_reading) break;
    if (!async_extend_line(s, &line, &used, &capacity, chunk + segment,
                           (size_t)amount - segment))
      break;
  }

  /* Match getline on ordinary EOF, but never publish a partial line after a
     cancellation request. */
  if (eof && used &&
      !atomic_load_explicit(&s->stop, memory_order_acquire))
    (void)async_process_candidate_line(s, line, used);

  free(line);
  bool stopping = atomic_load_explicit(&s->stop, memory_order_acquire);
  if (s->pid > 0 && async_claim_child(s, AsyncChildReader)) {
    uint64_t producer_error = atomic_load_explicit(
        &s->producer_error, memory_order_acquire);
    enum AsyncProducerErrorKind error_kind =
        async_unpack_producer_error_kind(producer_error);
    if (!stopping && error_kind != AsyncProducerErrorNone)
      async_signal_child(s, SIGKILL);

    int wait_status = 0;
    pid_t waited = async_wait_child(s, &wait_status);
    int wait_errno = errno;
    stopping = atomic_load_explicit(&s->stop, memory_order_acquire);

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
          s, AsyncProducerErrorWait, wait_errno ? wait_errno : ECHILD);
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
  } else if (s->pid <= 0) {
    uint64_t producer_error = atomic_load_explicit(
        &s->producer_error, memory_order_acquire);
    if (stopping)
      atomic_store_explicit(&s->producer_state, AsyncProducerStopped,
                            memory_order_release);
    else if (producer_error != 0)
      atomic_store_explicit(&s->producer_state, AsyncProducerFailed,
                            memory_order_release);
    else
      atomic_store_explicit(&s->producer_state, AsyncProducerComplete,
                            memory_order_release);
  }
  async_publish_producer_terminal(s);
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
  fzf_log("async_session_destroy: pid=%d\n", (int)s->pid);

  /* Signal everything to stop simultaneously so scoring and reader wind down
     in parallel rather than sequentially. */
  atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);
  async_worker_pool_wake(atomic_load_explicit(
      &s->worker_pool, memory_order_acquire));
  async_publish_stop(s);
  bool teardown_owns_child =
      s->pid > 0 && async_claim_child(s, AsyncChildTeardown);
  /* Synchronous startup unwind and the detached-worker fallback must also be
     bounded when the shell ignores TERM.  SIGKILL closes inherited pipes and
     matches the ordinary public async-stop path. */
  async_signal_child(s, SIGKILL);
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

  struct AsyncWorkerPool *worker_pool = atomic_exchange_explicit(
      &s->worker_pool, NULL, memory_order_acq_rel);
  if (s->worker_pool_owned)
    async_worker_pool_destroy(worker_pool);

  /* The reader signals score_req_cond when it appends a candidate.  Join it
     before the request mutex and condition variable are destroyed. */
  if (s->reader_started)
    pthread_join(s->reader, NULL);
  fzf_log("async_session_destroy: reader joined count=%zu\n", s->count);

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
  if (s->cancel_pipe_ready) {
    close(s->cancel_read_fd);
    close(s->cancel_write_fd);
    s->cancel_pipe_ready = false;
  }
  if (teardown_owns_child) {
    int status = 0;
    async_wait_child(s, &status);
  }
  pthread_mutex_lock(&s->mu);
  arena_free(&s->arena);
  for (size_t k = 0; k < CANDS_TOP_CAP; k++)
    if (s->cands_top[k]) { free(s->cands_top[k]); s->cands_top[k] = NULL; }
  pthread_mutex_unlock(&s->mu);
  pthread_mutex_destroy(&s->child_mu);
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

   The private cancellation pipe wakes the reader even when an escaped
   descendant keeps stdout open.  `kill(-pid, SIGKILL)` is independent
   best-effort process-group cleanup, including when the reader already owns
   waitpid.  The scoring thread short-circuits on score_abort.  The detached
   worker then joins both threads without holding up the Emacs main thread on
   minibuffer dismissal.

   Falls back to a synchronous destroy if pthread_create fails — the
   join cost is preferable to a leak. */
static void async_session_destroy_async(void *ptr) {
  AsyncSession *s = ptr;
  if (!s) return;
  fzf_log("async_session_destroy_async: pid=%d\n", (int)s->pid);

  atomic_store_explicit(&s->score_abort, true, memory_order_seq_cst);
  async_worker_pool_wake(atomic_load_explicit(
      &s->worker_pool, memory_order_acquire));
  async_publish_stop(s);
  async_signal_child(s, SIGKILL);
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

enum AsyncSpawnStage {
  AsyncSpawnStageProtocol = 1,
  AsyncSpawnStageStdout,
  AsyncSpawnStageStderr,
  AsyncSpawnStageDirectory,
  AsyncSpawnStageExec,
};

struct AsyncSpawnFailure {
  uint32_t stage;
  int error_number;
};

/* Child side of the close-on-exec status pipe.  Keep this path limited to
   async-signal-safe operations: after fork, another Emacs thread may have
   held libc's allocator or environment locks. */
static void async_child_spawn_fail(int status_fd,
                                   enum AsyncSpawnStage stage,
                                   int error_number) {
  struct AsyncSpawnFailure failure = {
    .stage = (uint32_t)stage,
    .error_number = error_number,
  };
  const unsigned char *bytes = (const unsigned char *)&failure;
  size_t written = 0;
  while (written < sizeof failure) {
    ssize_t amount = write(status_fd, bytes + written,
                           sizeof failure - written);
    if (amount > 0) {
      written += (size_t)amount;
      continue;
    }
    if (amount < 0 && errno == EINTR) continue;
    break;
  }
  _exit(127);
}

static const char *async_spawn_stage_name(uint32_t stage) {
  switch ((enum AsyncSpawnStage)stage) {
  case AsyncSpawnStageStdout: return "stdout redirection";
  case AsyncSpawnStageStderr: return "stderr redirection";
  case AsyncSpawnStageDirectory: return "working-directory change";
  case AsyncSpawnStageExec: return "shell exec";
  default: return "spawn-status protocol";
  }
}

/* Return true when the child reported a setup failure.  EOF before any bytes
   is success: FD_CLOEXEC closed the status writer atomically with execve. */
static bool async_read_spawn_failure(int status_fd,
                                     struct AsyncSpawnFailure *failure) {
  unsigned char *bytes = (unsigned char *)failure;
  size_t received = 0;
  while (received < sizeof *failure) {
    ssize_t amount = read(status_fd, bytes + received,
                          sizeof *failure - received);
    if (amount > 0) {
      received += (size_t)amount;
      continue;
    }
    if (amount < 0 && errno == EINTR) continue;
    if (amount == 0 && received == 0) return false;
    failure->stage = AsyncSpawnStageProtocol;
    failure->error_number = amount < 0 ? errno : EIO;
    return true;
  }
  return true;
}

static void async_signal_spawn_failure(
    emacs_env *env, const struct AsyncSpawnFailure *failure) {
  char message[256];
  int error_number = failure->error_number > 0
                         ? failure->error_number : EIO;
  snprintf(message, sizeof message,
           "fzf-native: producer %s failed: %s",
           async_spawn_stage_name(failure->stage), strerror(error_number));
  async_signal_error(env, message);
}

static bool async_copy_lisp_string(emacs_env *env, emacs_value value,
                                   const char *what, struct Str *out) {
  *out = (struct Str){0};
  ptrdiff_t length = 0;
  if (!env->copy_string_contents(env, value, NULL, &length)) return false;
  if (length <= 0) {
    async_signal_error(env, "fzf-native: invalid string length");
    return false;
  }
  char *copy = malloc((size_t)length);
  if (!copy) {
    async_signal_error(env, "fzf-native: could not allocate a string copy");
    return false;
  }
  if (!env->copy_string_contents(env, value, copy, &length)) {
    free(copy);
    return false;
  }
  if (length <= 0) {
    free(copy);
    async_signal_error(env, "fzf-native: invalid copied string length");
    return false;
  }
  /* copy_string_contents reports the trailing C NUL in LENGTH.  Every value
     copied here is subsequently passed to a C/POSIX string API, so an earlier
     NUL would silently change the command, directory, shell option, or PATH.
     Reject it while the original byte boundary is still known. */
  struct Str copied = {.b = copy, .len = (size_t)length - 1};
  if (reject_embedded_nul(env, copied, what)) {
    free(copy);
    return false;
  }
  *out = copied;
  return true;
}

static char *async_copy_lisp_c_string(emacs_env *env, emacs_value value,
                                      const char *what) {
  struct Str copied;
  return async_copy_lisp_string(env, value, what, &copied)
             ? copied.b : NULL;
}

/* Build an execve environment in the parent.  Only the pointer vector and
   replacement PATH entry are owned; unchanged entries point into environ and
   are stable in the fork snapshot. */
static bool async_build_child_environment(
    const char *exec_path_prefix, char ***out_environment,
    char ***out_owned_vector, char **out_owned_path) {
  *out_environment = environ;
  *out_owned_vector = NULL;
  *out_owned_path = NULL;
  if (!exec_path_prefix) return true;

  const char *old_path = getenv("PATH");
  size_t prefix_len = strlen(exec_path_prefix);
  size_t old_len = old_path && *old_path ? strlen(old_path) : 0;
  if (prefix_len > SIZE_MAX - old_len - 7) return false;
  size_t path_len = 5 + prefix_len + (old_len ? 1 + old_len : 0);
  char *path_entry = malloc(path_len + 1);
  if (!path_entry) return false;
  memcpy(path_entry, "PATH=", 5);
  memcpy(path_entry + 5, exec_path_prefix, prefix_len);
  size_t position = 5 + prefix_len;
  if (old_len) {
    path_entry[position++] = ':';
    memcpy(path_entry + position, old_path, old_len);
    position += old_len;
  }
  path_entry[position] = '\0';

  size_t count = 0;
  while (environ && environ[count]) count++;
  if (count > (SIZE_MAX / sizeof(char *)) - 2) {
    free(path_entry);
    return false;
  }
  char **environment = calloc(count + 2, sizeof *environment);
  if (!environment) {
    free(path_entry);
    return false;
  }
  bool replaced = false;
  size_t output = 0;
  for (size_t i = 0; i < count; i++) {
    if (!replaced && strncmp(environ[i], "PATH=", 5) == 0) {
      environment[output++] = path_entry;
      replaced = true;
    } else {
      environment[output++] = environ[i];
    }
  }
  if (!replaced) environment[output++] = path_entry;
  environment[output] = NULL;
  *out_environment = environment;
  *out_owned_vector = environment;
  *out_owned_path = path_entry;
  return true;
}

/* fzf-native-async-start COMMAND &optional DIR -> session handle */
static emacs_value
fzf_native_async_start(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *UNUSED(data)) {
  /* Establish Lisp ownership while it is still inert.  make_user_ptr has two
     allocation stages inside Emacs; publishing a live session first can leak
     it on the first failure or double-finalize it on the second.  A NULL
     payload makes either failure harmless, and set_user_ptr performs the
     non-allocating ownership transfer only after startup is complete. */
  emacs_value handle;
  if (!fzf_native_make_inert_user_ptr(
          env, async_session_destroy_async, &handle))
    return Qnil;

  char *cmd = async_copy_lisp_c_string(env, args[0], "producer command");
  if (!cmd) return Qnil;

  char *dir = NULL;
  if (nargs > 1 && !env->eq(env, args[1], Qnil)) {
    dir = async_copy_lisp_c_string(env, args[1], "producer directory");
    if (!dir) {
      free(cmd);
      return Qnil;
    }
  }

  /* Use shell-file-name / shell-command-switch so behaviour matches
     shell-command (M-!) rather than hardcoding /bin/sh -c. */
  char *shell_prog = NULL, *shell_switch = NULL;
  char *exec_path_str = NULL;
  {
    emacs_value v = defcustom_value(env, Qsym_shell_file_name, Qnil);
    if (!env->eq(env, v, Qnil)) {
      /* `shell-file-name' is allowed to be a bare file name.  Resolve it in
         the parent through Emacs so the same dynamic `exec-path' that
         shell-command uses is honored, while the post-fork child remains
         restricted to async-signal-safe setup plus execve. */
      emacs_value executable_find = env->intern(env, "executable-find");
      emacs_value resolved = env->funcall(env, executable_find, 1, &v);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        goto start_input_failure;
      if (env->eq(env, resolved, Qnil)) {
        async_signal_error(
            env, "fzf-native: shell-file-name is not executable in exec-path");
        goto start_input_failure;
      }
      shell_prog = async_copy_lisp_c_string(env, resolved, "shell-file-name");
      if (!shell_prog) goto start_input_failure;
    }
    if (!shell_prog) shell_prog = strdup("/bin/sh");
    if (!shell_prog) goto start_allocation_failure;
  }
  {
    emacs_value v = defcustom_value(env, Qsym_shell_command_switch, Qnil);
    if (!env->eq(env, v, Qnil)) {
      shell_switch = async_copy_lisp_c_string(
          env, v, "shell-command-switch");
      if (!shell_switch) goto start_input_failure;
    }
    if (!shell_switch) shell_switch = strdup("-c");
    if (!shell_switch) goto start_allocation_failure;
  }

  /* Build PATH from exec-path so the child shell can find binaries that
     Emacs can find, even on macOS GUI launches with a minimal inherited PATH. */
  {
    emacs_value v = defcustom_value(env, Qsym_exec_path, Qnil);
    if (!env->eq(env, v, Qnil)) {
      emacs_value sep    = env->make_string(env, ":", 1);
      emacs_value id     = env->intern(env, "identity");
      emacs_value mc_fn  = env->intern(env, "mapconcat");
      emacs_value mc_args[3] = {id, v, sep};
      emacs_value joined = env->funcall(env, mc_fn, 3, mc_args);
      if (env->non_local_exit_check(env) == emacs_funcall_exit_return) {
        exec_path_str = async_copy_lisp_c_string(env, joined, "exec-path");
        if (!exec_path_str) goto start_input_failure;
      }
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        goto start_input_failure;
    }
  }

  char **child_environment = NULL;
  char **owned_environment = NULL;
  char *owned_path = NULL;
  if (!async_build_child_environment(
          exec_path_str, &child_environment,
          &owned_environment, &owned_path))
    goto start_allocation_failure;

  fzf_log("async_start: shell='%s' switch='%s' cmd='%s' dir='%s' PATH='%s'\n",
          shell_prog, shell_switch, cmd, dir ? dir : "(nil)",
          exec_path_str ? exec_path_str : "(inherited)");

  int pfd[2];
  if (pipe(pfd) != 0) {
    int error_number = errno;
    fzf_log("async_start: output pipe failed: %s\n",
            strerror(error_number));
    free(cmd);
    free(dir);
    free(shell_prog);
    free(shell_switch);
    free(exec_path_str);
    free(owned_environment);
    free(owned_path);
    async_signal_posix_error(
        env, "producer output pipe creation", error_number);
    return Qnil;
  }

  int status_fd[2];
  if (pipe(status_fd) != 0) {
    int error_number = errno;
    close(pfd[0]);
    close(pfd[1]);
    free(cmd);
    free(dir);
    free(shell_prog);
    free(shell_switch);
    free(exec_path_str);
    free(owned_environment);
    free(owned_path);
    async_signal_posix_error(
        env, "producer status pipe creation", error_number);
    return Qnil;
  }
  int status_flags = fcntl(status_fd[1], F_GETFD);
  if (status_flags < 0 ||
      fcntl(status_fd[1], F_SETFD, status_flags | FD_CLOEXEC) < 0) {
    int error_number = errno;
    close(status_fd[0]);
    close(status_fd[1]);
    close(pfd[0]);
    close(pfd[1]);
    free(cmd);
    free(dir);
    free(shell_prog);
    free(shell_switch);
    free(exec_path_str);
    free(owned_environment);
    free(owned_path);
    async_signal_posix_error(
        env, "producer status pipe configuration", error_number);
    return Qnil;
  }

  pid_t pid = fork();
  if (pid < 0) {
    int error_number = errno;
    fzf_log("async_start: fork failed: %s\n", strerror(error_number));
    close(pfd[0]);
    close(pfd[1]);
    close(status_fd[0]);
    close(status_fd[1]);
    free(cmd);
    free(dir);
    free(shell_prog);
    free(shell_switch);
    free(exec_path_str);
    free(owned_environment);
    free(owned_path);
    async_signal_posix_error(
        env, "producer fork", error_number);
    return Qnil;
  }

  if (pid == 0) {
    fzf_prepare_forked_child_signals();
    /* Own process group: stop must also terminate descendants that inherited
       the output pipe.  Ignore failure here; the parent repeats setpgid and
       signalling has a direct-child fallback. */
    (void)setpgid(0, 0);
    close(status_fd[0]);
    close(pfd[0]);
    if (dup2(pfd[1], STDOUT_FILENO) < 0)
      async_child_spawn_fail(
          status_fd[1], AsyncSpawnStageStdout, errno);
    close(pfd[1]);
    int dn = open("/dev/null", O_WRONLY);
    if (dn < 0)
      async_child_spawn_fail(
          status_fd[1], AsyncSpawnStageStderr, errno);
    if (dup2(dn, STDERR_FILENO) < 0)
      async_child_spawn_fail(
          status_fd[1], AsyncSpawnStageStderr, errno);
    close(dn);
    if (dir && chdir(dir) < 0)
      async_child_spawn_fail(
          status_fd[1], AsyncSpawnStageDirectory, errno);
    char *const child_argv[] = {shell_prog, shell_switch, cmd, NULL};
    execve(shell_prog, child_argv, child_environment);
    async_child_spawn_fail(status_fd[1], AsyncSpawnStageExec, errno);
  }
  (void)setpgid(pid, pid);
  close(pfd[1]);
  close(status_fd[1]);
  struct AsyncSpawnFailure spawn_failure = {0};
  bool spawn_failed = async_read_spawn_failure(
      status_fd[0], &spawn_failure);
  close(status_fd[0]);
  free(shell_prog);
  free(shell_switch);
  free(exec_path_str);
  free(owned_environment);
  free(owned_path);
  if (spawn_failed) {
    close(pfd[0]);
    (void)waitpid(pid, NULL, 0);
    async_signal_spawn_failure(env, &spawn_failure);
    free(cmd);
    free(dir);
    return Qnil;
  }

  AsyncSession *s = calloc(1, sizeof *s);
  if (!s) {
    fzf_log("async_start: calloc failed\n");
    close(pfd[0]);
    async_kill_and_reap_spawned_child(pid);
    async_signal_error(env, "fzf-native: could not allocate async session");
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
  pthread_mutex_init(&s->child_mu, NULL);
  pthread_mutex_init(&s->score_req_mu, NULL);
  pthread_cond_init(&s->score_req_cond, NULL);
  pthread_mutex_init(&s->score_res_mu, NULL);
  atomic_store(&s->gen, 0);
  atomic_store(&s->score_abort, false);
  atomic_init(&s->worker_pool, NULL);
  atomic_store(&s->child_owner, AsyncChildUnclaimed);
  s->child_alive = true;
  atomic_store(&s->producer_state, AsyncProducerRunning);
  atomic_store(&s->producer_error, 0);
  atomic_store(&s->producer_exit_status, -1);

  bool cancel_ready = async_make_cancel_pipe(s);

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
    size_t cache_max_bytes = 64 * 1024 * 1024;
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
    emacs_value bytes_val = defcustom_value(
        env, Qsym_async_cache_bytes, Qnil);
    if (!env->eq(env, bytes_val, Qnil)) {
      intmax_t n = env->extract_integer(env, bytes_val);
      if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        env->non_local_exit_clear(env);
      else if (n >= 0)
        cache_max_bytes = (size_t)n;
    }
    cache_init_limits(&s->cache, cache_max, cache_max_bytes);
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

  /* Worker threads are allocated lazily by the first scoring request through
     one process-wide pool.  Starting many idle sessions therefore adds only
     their reader/scorer coordinators, not N × CPU persistent workers. */
  bool start_ok = s->fp != NULL && cancel_ready;
  const char *start_error = s->fp == NULL
      ? "fzf-native: could not open producer output"
      : cancel_ready ? NULL : "fzf-native: could not create reader wakeup";
  if (start_ok) {
    start_ok = pthread_create(&s->reader, NULL, async_reader, s) == 0;
    s->reader_started = start_ok;
    if (!start_ok)
      start_error = "fzf-native: could not start producer reader";
  }
  if (start_ok) {
    start_ok = pthread_create(&s->score_thread, NULL,
                              scoring_thread_fn, s) == 0;
    s->score_thread_started = start_ok;
    if (!start_ok)
      start_error = "fzf-native: could not start scoring coordinator";
  }
  if (!start_ok) {
    async_session_destroy(s);
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
      async_signal_error(env, start_error);
    return Qnil;
  }
  /* The user_ptr finalizer (GC sweep on Emacs main thread) routes through
     the async path too: signaling + pthread_create are O(µs), so GC stays
     fast and the blocking pthread_join runs off-main. */
  env->set_user_ptr(env, handle, s);
  return handle;

start_allocation_failure:
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    async_signal_error(env, "fzf-native: could not allocate producer setup");
start_input_failure:
  free(cmd);
  free(dir);
  free(shell_prog);
  free(shell_switch);
  free(exec_path_str);
  return Qnil;
}

/* fzf-native-async-stop HANDLE */
static emacs_value
fzf_native_async_stop(emacs_env *env, ptrdiff_t nargs,
                      emacs_value args[], void *UNUSED(data)) {
  (void)nargs;
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (s) {
    fzf_log("async_stop: pid=%d\n", (int)s->pid);
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
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (!s) return Qnil;
  return env->make_integer(env,
    atomic_load_explicit(&s->gen, memory_order_relaxed));
}

static int cmp_scored_desc(const void *a, const void *b) {
  const ScoredStr *left = a;
  const ScoredStr *right = b;
  if (left->score != right->score)
    return left->score > right->score ? -1 : 1;
  if (left->idx != right->idx)
    return left->idx < right->idx ? -1 : 1;
  return 0;
}

static bool async_heap_sift_scored(ScoredStr *xs, size_t root, size_t end,
                                   _Atomic bool *stop) {
  size_t checks = 0;
  for (;;) {
    if ((checks++ & 0xFF) == 0 && async_stop_requested(stop)) return false;
    if (root > (end - 1) / 2) return true;
    size_t child = root * 2 + 1;
    if (child >= end) return true;
    if (child + 1 < end &&
        cmp_scored_desc(xs + child, xs + child + 1) < 0)
      child++;
    if (cmp_scored_desc(xs + root, xs + child) >= 0) return true;
    ScoredStr swap = xs[root];
    xs[root] = xs[child];
    xs[child] = swap;
    root = child;
  }
}

/* Allocationless fallback for counting-sort scratch failure.  The root is
   the worst element under the public total order, so each extraction places
   that element at the end and leaves score-desc/index-ascending output. */
static bool async_heap_sort_scored(ScoredStr *xs, size_t n,
                                   _Atomic bool *stop) {
  if (n <= 1) return !async_stop_requested(stop);
  for (size_t start = n / 2; start > 0;) {
    start--;
    if (!async_heap_sift_scored(xs, start, n, stop)) return false;
  }
  for (size_t end = n; end > 1;) {
    end--;
    ScoredStr swap = xs[0];
    xs[0] = xs[end];
    xs[end] = swap;
    if (!async_heap_sift_scored(xs, 0, end, stop)) return false;
  }
  return !async_stop_requested(stop);
}

/* Counting sort of xs[0..n-1] by score, descending.
   O(n + max_score). Falls back to abortable allocationless heapsort when
   scratch allocation fails. */
/* Return false when STOP was raised while sorting.  The ordinary test and
   batch APIs use the wrapper below with no stop flag. */
static bool counting_sort_scored_abortable(ScoredStr *xs, size_t n,
                                            _Atomic bool *stop) {
  if (n <= 1) return !async_stop_requested(stop);
  int max_score = 0;
  for (size_t i = 0; i < n; i++) {
    if ((i & 0x3FFF) == 0 && async_stop_requested(stop)) return false;
    if (xs[i].score > max_score) max_score = xs[i].score;
  }

  int *count = calloc((size_t)(max_score + 1), sizeof *count);
  if (!count) return async_heap_sort_scored(xs, n, stop);

  for (size_t i = 0; i < n; i++) {
    if ((i & 0x3FFF) == 0 && async_stop_requested(stop)) {
      free(count);
      return false;
    }
    count[xs[i].score]++;
  }

  /* Convert counts to start positions for descending order. */
  int pos = 0;
  for (int s = max_score; s >= 0; s--) {
    if (((unsigned)s & 0x3FFF) == 0 && async_stop_requested(stop)) {
      free(count);
      return false;
    }
    int c = count[s];
    count[s] = pos;
    pos += c;
  }

  ScoredStr *out = malloc(n * sizeof *out);
  if (!out) {
    free(count);
    return async_heap_sort_scored(xs, n, stop);
  }

  for (size_t i = 0; i < n; i++) {
    if ((i & 0x3FFF) == 0 && async_stop_requested(stop)) {
      free(out);
      free(count);
      return false;
    }
    out[count[xs[i].score]++] = xs[i];
  }
  for (size_t base = 0; base < n; base += 16384) {
    if (async_stop_requested(stop)) {
      free(out);
      free(count);
      return false;
    }
    size_t amount = MIN((size_t)16384, n - base);
    memcpy(xs + base, out + base, amount * sizeof *xs);
  }
  free(out);
  free(count);
  return !async_stop_requested(stop);
}

static void counting_sort_scored(ScoredStr *xs, size_t n) {
  (void)counting_sort_scored_abortable(xs, n, NULL);
}

/* Merge two ranked arrays into the first LIMIT results.  Score descending and
   candidate index ascending is fzf-native's stable public ordering. */
static size_t async_merge_top_k(const ScoredStr *left, size_t left_count,
                                const ScoredStr *right, size_t right_count,
                                size_t limit, ScoredStr *out,
                                _Atomic bool *stop) {
  size_t li = 0, ri = 0, oi = 0;
  while (oi < limit && (li < left_count || ri < right_count)) {
    if ((oi & 0x3FFF) == 0 && async_stop_requested(stop)) return SIZE_MAX;
    if (ri == right_count ||
        (li < left_count && cmp_scored_desc(left + li, right + ri) <= 0))
      out[oi++] = left[li++];
    else
      out[oi++] = right[ri++];
  }
  return oi;
}

typedef struct {
  uint32_t *idx;
  size_t count;
  size_t capacity;
  size_t max_count;
  bool enabled;
} AsyncMembershipBuilder;

static void async_membership_disable(AsyncMembershipBuilder *builder) {
  free(builder->idx);
  builder->idx = NULL;
  builder->count = 0;
  builder->capacity = 0;
  builder->enabled = false;
}

static bool async_membership_reserve(AsyncMembershipBuilder *builder,
                                     size_t needed) {
  if (!builder->enabled) return false;
  if (needed > builder->max_count) {
    async_membership_disable(builder);
    return false;
  }
  if (needed <= builder->capacity) return true;
  size_t next = builder->capacity ? builder->capacity : 1024;
  if (next > builder->max_count) next = builder->max_count;
  while (next < needed) {
    size_t increment = next / 2;
    if (increment == 0) increment = 1;
    if (next > builder->max_count - increment) {
      next = builder->max_count;
      break;
    }
    next += increment;
  }
  if (next < needed || next > SIZE_MAX / sizeof *builder->idx) {
    async_membership_disable(builder);
    return false;
  }
  uint32_t *grown = realloc(builder->idx, next * sizeof *grown);
  if (!grown) {
    async_membership_disable(builder);
    return false;
  }
  builder->idx = grown;
  builder->capacity = next;
  return true;
}

static void async_membership_append(AsyncMembershipBuilder *builder,
                                    const ScoredStr *values, size_t count,
                                    _Atomic bool *stop) {
  if (!builder->enabled || count == 0) return;
  if (count > SIZE_MAX - builder->count ||
      !async_membership_reserve(builder, builder->count + count))
    return;
  for (size_t i = 0; i < count; i++) {
    if ((i & 0x3FFF) == 0 && async_stop_requested(stop)) return;
    builder->idx[builder->count++] = values[i].idx;
  }
}

static void async_membership_append_indices(
    AsyncMembershipBuilder *builder, const uint32_t *values, size_t count,
    _Atomic bool *stop) {
  if (!builder->enabled || count == 0) return;
  if (!values || count > SIZE_MAX - builder->count ||
      !async_membership_reserve(builder, builder->count + count))
    return;
  for (size_t base = 0; base < count; base += 16384) {
    if (async_stop_requested(stop)) return;
    size_t amount = MIN((size_t)16384, count - base);
    memcpy(builder->idx + builder->count, values + base,
           amount * sizeof *values);
    builder->count += amount;
  }
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
  /* Process-wide pool contention flag.  Workers normally drain batches in
     one epoch, but yield after their current batch when another session is
     waiting.  NULL for direct/fallback execution. */
  _Atomic bool             *yield_after_batch;
  BatchCache               *batch_cache;
  BatchQuery               *target_query;
  /* When true, workers replace fzf_get_score with fzf_has_match (boolean
     match-only check from fzf-additions).  The compaction logic is
     identical; the score field is just set to 0 (unscored) and the
     calling thread skips counting_sort_scored. */
  bool                      filter_only;
  _Atomic bool              allocation_failed;
};

/* Grow the compact result array in checked geometric steps.  The coordinator
   materializes only ASYNC_BATCH_WINDOW input batches at a time, so this array
   is the only storage whose size follows the number of actual matches. */
static bool async_scored_reserve(ScoredStr **values, size_t *capacity,
                                 size_t needed) {
  if (needed <= *capacity) return true;
  const size_t max_capacity = SIZE_MAX / sizeof **values;
  if (needed > max_capacity) return false;

  size_t next = *capacity ? *capacity : MIN((size_t)BATCH_SIZE, max_capacity);
  while (next < needed) {
    size_t increment = next / 2;
    if (increment == 0) increment = 1;
    if (next > max_capacity - increment) {
      next = max_capacity;
      break;
    }
    next += increment;
  }
  if (next < needed) return false;
  ScoredStr *grown = realloc(*values, next * sizeof *grown);
  if (!grown) return false;
  *values = grown;
  *capacity = next;
  return true;
}

struct AsyncWorkerContext;
struct AsyncWorkerWaiter;

struct AsyncWorkerPool {
  pthread_t *threads;
  struct AsyncWorkerContext *contexts;
  unsigned count;
  pthread_mutex_t mu;
  pthread_cond_t job_cond;
  pthread_cond_t done_cond;
  bool stop;
  _Atomic bool contended;
  uint64_t epoch;
  unsigned active;
  struct AsyncScoringShared *job;
  struct AsyncWorkerWaiter *wait_head;
  struct AsyncWorkerWaiter *wait_tail;
  size_t waiting;
};

struct AsyncWorkerContext {
  struct AsyncWorkerPool *pool;
};

struct AsyncWorkerWaiter {
  struct AsyncWorkerWaiter *prev;
  struct AsyncWorkerWaiter *next;
  struct AsyncScoringShared *job;
  bool queued;
};

/* Score at most QUOTA batches.  A zero quota normally drains the job.  A
   persistent worker also yields after its current batch when another session
   has joined the process-wide pool queue. */
static void async_score_batches(struct AsyncScoringShared *shared,
                                fzf_slab_t *slab, size_t quota) {
  fzf_pattern_t *pattern      = shared->pattern;
  bool           filter_only  = shared->filter_only;

  ssize_t bi;
  size_t claimed = 0;
  while ((quota == 0 || claimed < quota) &&
         (bi = atomic_fetch_sub_explicit(&shared->remaining, 1,
                                         memory_order_seq_cst) - 1) >= 0) {
    claimed++;
    if (shared->stop && atomic_load_explicit(shared->stop, memory_order_relaxed))
      break;
    struct AsyncScoringBatch *batch = shared->batches + bi;
    unsigned original_len = batch->len;
    unsigned n = 0;
    bool aborted = false;
    for (unsigned i = 0; i < batch->len; i++) {
      if (atomic_load_explicit(&shared->allocation_failed,
                               memory_order_relaxed)) {
        aborted = true;
        break;
      }
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
      if (fzf_allocation_failed()) {
        atomic_store_explicit(&shared->allocation_failed, true,
                              memory_order_relaxed);
        aborted = true;
        break;
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
    if (shared->yield_after_batch &&
        atomic_load_explicit(shared->yield_after_batch,
                             memory_order_acquire))
      break;
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

    async_score_batches(job, slab, 0);

    pthread_mutex_lock(&pool->mu);
    if (pool->active > 0 && --pool->active == 0)
      /* Wake both the job owner and coordinators waiting to submit.  A
         signal can wake only a submitter, which observes JOB still busy and
         sleeps again while the owner remains asleep forever. */
      pthread_cond_broadcast(&pool->done_cond);
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
  atomic_init(&pool->contended, false);

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

static void async_worker_pool_update_contention_locked(
    struct AsyncWorkerPool *pool) {
  /* If a job is active, every queued waiter is a competitor.  Between
     epochs, one waiter is about to become the active owner and only the
     remaining waiters should make that next epoch yield. */
  bool contended = pool->job ? pool->waiting > 0 : pool->waiting > 1;
  atomic_store_explicit(&pool->contended, contended, memory_order_release);
}

static void async_worker_waiter_push_locked(
    struct AsyncWorkerPool *pool, struct AsyncWorkerWaiter *waiter) {
  if (waiter->queued) return;
  waiter->prev = pool->wait_tail;
  waiter->next = NULL;
  if (pool->wait_tail) pool->wait_tail->next = waiter;
  else pool->wait_head = waiter;
  pool->wait_tail = waiter;
  pool->waiting++;
  waiter->queued = true;
  async_worker_pool_update_contention_locked(pool);
}

static void async_worker_waiter_unlink_locked(
    struct AsyncWorkerPool *pool, struct AsyncWorkerWaiter *waiter) {
  if (!waiter->queued) return;
  if (waiter->prev) waiter->prev->next = waiter->next;
  else pool->wait_head = waiter->next;
  if (waiter->next) waiter->next->prev = waiter->prev;
  else pool->wait_tail = waiter->prev;
  waiter->prev = waiter->next = NULL;
  if (pool->waiting > 0) pool->waiting--;
  waiter->queued = false;
  async_worker_pool_update_contention_locked(pool);
}

static bool async_scoring_job_cancelled(
    const struct AsyncScoringShared *job) {
  return job->stop &&
      atomic_load_explicit(job->stop, memory_order_relaxed);
}

/* Wake coordinators queued on the shared pool after their session's abort
   flag changes.  Without this notification a cancelled session could retain
   its scorer and all session memory until an unrelated long job completed. */
static void async_worker_pool_wake(struct AsyncWorkerPool *pool) {
  if (!pool) return;
  pthread_mutex_lock(&pool->mu);
  pthread_cond_broadcast(&pool->done_cond);
  pthread_mutex_unlock(&pool->mu);
}

static void async_worker_pool_run(struct AsyncWorkerPool *pool,
                                  struct AsyncScoringShared *job) {
  if (!pool || !job || pool->count == 0) return;
  struct AsyncWorkerWaiter waiter = {.job = job};
  pthread_mutex_lock(&pool->mu);
  if (pool->stop || async_scoring_job_cancelled(job)) {
    pthread_mutex_unlock(&pool->mu);
    return;
  }
  async_worker_waiter_push_locked(pool, &waiter);
  job->yield_after_batch = &pool->contended;
  for (;;) {
    while (!pool->stop && !async_scoring_job_cancelled(job) &&
           (pool->job != NULL || pool->wait_head != &waiter)) {
      /* Abort stores normally call async_worker_pool_wake.  A short timed wait
         also makes the queue self-cancelling for test callers and for any
         future abort path that cannot safely reach the pool notification
         helper. */
      struct timespec deadline;
      clock_gettime(CLOCK_REALTIME, &deadline);
      deadline.tv_nsec += 10 * 1000 * 1000;
      if (deadline.tv_nsec >= 1000 * 1000 * 1000) {
        deadline.tv_sec++;
        deadline.tv_nsec -= 1000 * 1000 * 1000;
      }
      (void)pthread_cond_timedwait(&pool->done_cond, &pool->mu, &deadline);
    }
    if (pool->stop || async_scoring_job_cancelled(job)) {
      async_worker_waiter_unlink_locked(pool, &waiter);
      job->yield_after_batch = NULL;
      pthread_cond_broadcast(&pool->done_cond);
      pthread_mutex_unlock(&pool->mu);
      return;
    }

    /* Publish one adaptive fair quantum.  With no competing waiter, workers
       drain the request in the original single epoch.  If another session
       arrives, CONTENDED becomes true and each worker yields after its
       current batch.  The unfinished owner then rejoins the FIFO tail.  There
       is still only one published descriptor per epoch, which preserves the
       lifetime and no-overwrite guarantees of the original serialized pool. */
    async_worker_waiter_unlink_locked(pool, &waiter);
    pool->job = job;
    async_worker_pool_update_contention_locked(pool);
    pool->active = pool->count;
    pool->epoch++;
    pthread_cond_broadcast(&pool->job_cond);
    while (pool->active > 0)
      pthread_cond_wait(&pool->done_cond, &pool->mu);
    pool->job = NULL;
    async_worker_pool_update_contention_locked(pool);

    bool complete = atomic_load_explicit(&job->remaining,
                                         memory_order_acquire) <= 0 ||
                    atomic_load_explicit(&job->allocation_failed,
                                         memory_order_acquire);
    if (pool->stop || async_scoring_job_cancelled(job) || complete) {
      job->yield_after_batch = NULL;
      pthread_cond_broadcast(&pool->done_cond);
      pthread_mutex_unlock(&pool->mu);
      return;
    }

    async_worker_waiter_push_locked(pool, &waiter);
    pthread_cond_broadcast(&pool->done_cond);
  }
}

static void async_worker_pool_destroy(struct AsyncWorkerPool *pool) {
  if (!pool) return;
  pthread_mutex_lock(&pool->mu);
  pool->stop = true;
  pthread_cond_broadcast(&pool->job_cond);
  pthread_cond_broadcast(&pool->done_cond);
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

static pthread_once_t async_global_worker_once = PTHREAD_ONCE_INIT;
static struct AsyncWorkerPool *async_global_workers;

static void async_global_worker_pool_init(void) {
  long detected_cpus = sysconf(_SC_NPROCESSORS_ONLN);
  unsigned count = fzf_worker_count(detected_cpus, 0);
  async_global_workers = async_worker_pool_create(count);
}

static struct AsyncWorkerPool *async_global_worker_pool(void) {
  pthread_once(&async_global_worker_once, async_global_worker_pool_init);
  return async_global_workers;
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
  if (!filter) {
    /* Consuming score_growth_pending without either queueing work or
       publishing a terminal state leaves a quiescent producer's latest
       request permanently stale.  Report the allocation failure against
       the captured logical request instead; a concurrently newer request
       has a different ID and will not observe this error as its own. */
    if (request_id != 0)
      async_publish_score_failure(
          s, request_id,
          "fzf-native: matcher could not allocate growth retry query");
    return false;
  }

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
    /* The growth event was already consumed above, but nothing was
       queued: a submit superseded this request inside the window.  If
       that submit took the exact-cache path it installed no request
       either, and the appended candidates would never be rescanned.
       Re-arm so the next idle wakeup re-evaluates against whatever is
       latest by then. */
    atomic_store_explicit(&s->score_growth_pending, true,
                          memory_order_release);
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

    /* Decide filter-only vs full-scoring mode for this run.  In
       filter-only the workers use fzf_has_match (cheap boolean check
       from fzf-additions).  The coordinator retains the first K matches
       in pool order, tracks the total separately, and records complete
       membership only while it fits the independent cache cap.

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

    /* An exact cached result with sufficient top-K capacity can extend over
       append-only growth without re-scoring its old matches.  Score only the
       new suffix, then merge it with the cached top-K and membership set.
       Filter-only results deliberately stay on the ordinary refinement path:
       their ranked input window is defined in producer order, which a cached
       score-sorted top-K does not retain. */
    ScoredStr *growth_top = NULL;
    size_t growth_top_count = 0, growth_pool_gen = 0;
    size_t growth_matched_count = 0;
    SharedIdx *growth_idx = NULL;
    bool growth_covered = false;
    bool incremental_growth = false;
    if (!filter_only_mode && refine_delta_from < count) {
      bool exact_growth = cache_lookup_exact_for_request(
          &s->cache, filter, case_mode, fuzzy, false, limit,
          &growth_top, &growth_top_count, &growth_idx, &growth_pool_gen,
          &growth_matched_count, &growth_covered);
      incremental_growth = exact_growth && growth_covered && growth_idx &&
                           growth_idx->count == growth_matched_count &&
                           growth_pool_gen == refine_delta_from;
      if (incremental_growth) {
        shared_idx_release(refine_idx);
        refine_idx = growth_idx;
        growth_idx = NULL;
      } else {
        free(growth_top);
        growth_top = NULL;
        growth_top_count = 0;
      }
      shared_idx_release(growth_idx);
    }

    /* Validate whole-result refinement evidence before using it.  Stored
       membership is sorted in producer order, contains only indices below
       its pool boundary, and is followed by the unseen append-only suffix.
       Any malformed/stale evidence falls back to a full scan. */
    bool use_refinement = refine_idx && refine_delta_from <= count;
    if (use_refinement) {
      for (size_t i = 0; i < refine_idx->count; i++) {
        if (refine_idx->idx[i] >= refine_delta_from ||
            (i > 0 && refine_idx->idx[i - 1] >= refine_idx->idx[i])) {
          use_refinement = false;
          break;
        }
      }
    }
    if (!use_refinement) {
      incremental_growth = false;
      free(growth_top);
      growth_top = NULL;
      growth_top_count = 0;
    }

    /* Select one safe cached ancestor for this query.  Each immutable full
       batch can reuse that ancestor independently; a cache miss scans the
       full batch.  The mutable final partial batch always scans in full. */
    BatchQuery *source_query = use_refinement ? NULL :
        batch_cache_select_source(&s->batch_cache, filter, case_mode, fuzzy);
    BatchQuery *target_query = use_refinement ? NULL :
        batch_cache_acquire_query(&s->batch_cache, filter, case_mode, fuzzy);
    size_t old_scan_count = use_refinement && !incremental_growth
                                ? refine_idx->count : 0;
    size_t delta_from = use_refinement ? refine_delta_from : 0;
    size_t scan_input_count = old_scan_count + (count - delta_from);
    atomic_store_explicit(&s->score_progress_total, scan_input_count,
                          memory_order_relaxed);
    size_t batch_count = scan_input_count
        ? (scan_input_count + BATCH_SIZE - 1) / BATCH_SIZE : 0;
    size_t batch_capacity = MIN(batch_count, (size_t)ASYNC_BATCH_WINDOW);
    struct AsyncScoringBatch *batches = batch_capacity
        ? calloc(batch_capacity, sizeof *batches) : NULL;
    size_t scan_count = 0;
    size_t reused_batches = 0;
    if (batch_capacity && !batches) {
      async_publish_score_failure(
          s, request_id, "matcher could not allocate batch storage");
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      shared_idx_release(refine_idx);
      free(growth_top);
      continue;
    }

    size_t flen = strlen(filter);
    char *pattern_query = flen ? strdup(filter) : NULL;
    if (flen && !pattern_query) {
      async_publish_score_failure(
          s, request_id, "matcher could not copy the query for parsing");
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      shared_idx_release(refine_idx);
      free(growth_top);
      continue;
    }
    fzf_pattern_t *pattern = flen
        ? fzf_parse_pattern(case_mode, false, pattern_query, fuzzy)
        : NULL;
    free(pattern_query);
    if (flen && !pattern) {
      async_publish_score_failure(
          s, request_id, "matcher could not allocate parsed query");
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      shared_idx_release(refine_idx);
      free(growth_top);
      continue;
    }
    bool has_pattern = (pattern != NULL);
    /* Materialize and score a bounded batch window at a time.  Positive-limit
       full-mode requests reduce every window to top-K, then merge it into a
       running top-K.  Filter-only requests retain the first K matches in
       producer order.  Total matches and optional refinement membership are
       tracked independently, so dense result sets no longer materialize one
       ScoredStr per match merely to discard the tail.  LIMIT 0 intentionally
       retains every result because the caller requested an unlimited result. */
    bool bounded_full = limit > 0 && !filter_only_mode;
    ScoredStr *flat = NULL;
    size_t flat_capacity = 0;
    size_t pos = 0;
    ScoredStr *window_values = NULL;
    size_t window_values_capacity = 0;
    ScoredStr *merge_values = NULL;
    size_t merge_values_capacity = 0;
    size_t delta_matches = 0;

    if (bounded_full && incremental_growth && growth_top_count) {
      flat = growth_top;
      growth_top = NULL;
      flat_capacity = growth_top_count;
      pos = growth_top_count;
    }

    size_t membership_budget = s->cache.max_bytes / 2;
    size_t membership_capacity = membership_budget > sizeof(SharedIdx)
        ? (membership_budget - sizeof(SharedIdx)) / sizeof(uint32_t) : 0;
    AsyncMembershipBuilder membership = {
      .max_count = membership_capacity,
      .enabled = strchr(filter, '|') == NULL && membership_capacity > 0,
    };
    if (incremental_growth && refine_idx)
      async_membership_append_indices(
          &membership, refine_idx->idx, refine_idx->count, &s->score_abort);

    bool score_allocation_failed = false;
    bool result_allocation_failed = false;
    bool aborted = false;
    struct AsyncWorkerPool *worker_pool = atomic_load_explicit(
        &s->worker_pool, memory_order_acquire);
    if (!worker_pool && batch_count) {
      worker_pool = async_global_worker_pool();
      atomic_store_explicit(
          &s->worker_pool, worker_pool, memory_order_release);
    }

    for (size_t window_base = 0; window_base < batch_count;
         window_base += batch_capacity) {
      size_t window_count = MIN(batch_capacity, batch_count - window_base);
      memset(batches, 0, window_count * sizeof *batches);

      for (size_t wi = 0; wi < window_count; wi++) {
        if (atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
          aborted = true;
          break;
        }
        size_t bi = window_base + wi;
        size_t start = bi * BATCH_SIZE;
        size_t available = MIN(BATCH_SIZE, scan_input_count - start);
        bool full_stable_batch = !use_refinement && available == BATCH_SIZE;
        uint16_t cached_local[BATCH_SIZE];
        size_t selected_count = 0;
        bool reused = full_stable_batch && source_query &&
            batch_cache_copy_members(&s->batch_cache, source_query, bi,
                                     cached_local, &selected_count);
        if (!reused) selected_count = available;

        struct AsyncScoringBatch *batch = &batches[wi];
        batch->batch_id = bi;
        batch->cacheable = full_stable_batch;
        batch->len = (unsigned)selected_count;
        pthread_mutex_lock(&s->mu);
        for (size_t local_i = 0; local_i < selected_count; local_i++) {
          size_t local = reused ? cached_local[local_i] : local_i;
          size_t ordinal = start + local;
          size_t global_i;
          if (use_refinement && ordinal < old_scan_count)
            global_i = refine_idx->idx[ordinal];
          else if (use_refinement)
            global_i = delta_from + (ordinal - old_scan_count);
          else
            global_i = ordinal;
          batch->xs[local_i].str =
              s->cands_top[global_i >> CANDS_BLOCK_SHIFT]
                          [global_i & CANDS_BLOCK_MASK];
          batch->xs[local_i].score = 0;
          batch->xs[local_i].idx = (uint32_t)global_i;
        }
        pthread_mutex_unlock(&s->mu);
        scan_count += selected_count;
        if (reused) {
          reused_batches++;
          /* Cache evidence has already resolved the candidates omitted from
             this narrower scan.  Count them as completed so progress has one
             fixed logical total and cannot report 100%, then fall backward
             when the coordinator prepares its next bounded window. */
          atomic_fetch_add_explicit(
              &s->score_progress_completed, available - selected_count,
              memory_order_relaxed);
        }
      }
      if (aborted) break;
      struct AsyncScoringShared shared = {
        .pattern     = pattern,
        .batches     = batches,
        .remaining   = (ssize_t)window_count,
        .stop        = &s->score_abort,
        .progress_completed = &s->score_progress_completed,
        .batch_cache = &s->batch_cache,
        .target_query = target_query,
        .filter_only = filter_only_mode,
        .allocation_failed = false,
      };
      if (worker_pool) {
        async_worker_pool_run(worker_pool, &shared);
      } else {
        /* Allocation failure for the shared pool is not a matcher failure;
           the coordinator can execute the identical batch loop itself. */
        fzf_slab_t *fallback_slab = fzf_make_default_slab();
        async_score_batches(&shared, fallback_slab, 0);
        if (fallback_slab) fzf_free_slab(fallback_slab);
      }

      if (atomic_load_explicit(&s->score_abort, memory_order_relaxed)) {
        aborted = true;
        break;
      }
      if (atomic_load_explicit(&shared.allocation_failed,
                               memory_order_relaxed)) {
        score_allocation_failed = true;
        break;
      }

      size_t window_matches = 0;
      for (size_t wi = 0; wi < window_count; wi++) {
        if (window_matches > SIZE_MAX - batches[wi].len) {
          result_allocation_failed = true;
          break;
        }
        window_matches += batches[wi].len;
      }
      if (result_allocation_failed ||
          window_matches > SIZE_MAX - delta_matches) {
        result_allocation_failed = true;
        break;
      }
      delta_matches += window_matches;

      for (size_t wi = 0; wi < window_count; wi++) {
        struct AsyncScoringBatch *batch = &batches[wi];
        async_membership_append(
            &membership, batch->xs, batch->len, &s->score_abort);
        if (async_stop_requested(&s->score_abort)) {
          aborted = true;
          break;
        }
      }
      if (aborted) break;

      if (limit == 0) {
        if (window_matches > SIZE_MAX - pos ||
            !async_scored_reserve(&flat, &flat_capacity,
                                  pos + window_matches)) {
          result_allocation_failed = true;
          break;
        }
        for (size_t wi = 0; wi < window_count; wi++) {
          struct AsyncScoringBatch *batch = &batches[wi];
          if (batch->len) {
            memcpy(flat + pos, batch->xs, batch->len * sizeof *flat);
            pos += batch->len;
          }
        }
      } else if (filter_only_mode) {
        size_t keep = MIN(window_matches, limit - pos);
        if (keep && !async_scored_reserve(
                        &flat, &flat_capacity, pos + keep)) {
          result_allocation_failed = true;
          break;
        }
        for (size_t wi = 0; wi < window_count && keep; wi++) {
          struct AsyncScoringBatch *batch = &batches[wi];
          size_t amount = MIN(keep, (size_t)batch->len);
          if (amount) {
            memcpy(flat + pos, batch->xs, amount * sizeof *flat);
            pos += amount;
            keep -= amount;
          }
        }
      } else if (window_matches) {
        if (!async_scored_reserve(&window_values,
                                  &window_values_capacity,
                                  window_matches)) {
          result_allocation_failed = true;
          break;
        }
        size_t window_pos = 0;
        for (size_t wi = 0; wi < window_count; wi++) {
          struct AsyncScoringBatch *batch = &batches[wi];
          if (batch->len) {
            memcpy(window_values + window_pos, batch->xs,
                   batch->len * sizeof *window_values);
            window_pos += batch->len;
          }
        }
        if (!counting_sort_scored_abortable(
                window_values, window_pos, &s->score_abort)) {
          aborted = true;
          break;
        }
        size_t window_keep = MIN(limit, window_pos);
        size_t merged_count = pos > limit - window_keep
                                  ? limit : pos + window_keep;
        if (merged_count && !async_scored_reserve(
                                &merge_values, &merge_values_capacity,
                                merged_count)) {
          result_allocation_failed = true;
          break;
        }
        size_t produced = async_merge_top_k(
            flat, pos, window_values, window_keep, limit,
            merge_values, &s->score_abort);
        if (produced == SIZE_MAX) {
          aborted = true;
          break;
        }
        ScoredStr *old_flat = flat;
        size_t old_flat_capacity = flat_capacity;
        flat = merge_values;
        flat_capacity = merge_values_capacity;
        merge_values = old_flat;
        merge_values_capacity = old_flat_capacity;
        pos = produced;
      }
    }

    /* `pattern' lifetime extends past every worker-pool rendezvous.  In
       filter-only mode we re-score the emit window below to recover ranked
       order within the displayed top-K. */
    if (aborted) {
      async_clear_current_request(s, request_id);
      if (pattern) fzf_free_pattern(pattern);
      free(flat);
      free(window_values);
      free(merge_values);
      free(membership.idx);
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      shared_idx_release(refine_idx);
      free(growth_top);
      continue;
    }
    if (score_allocation_failed || result_allocation_failed) {
      if (pattern) fzf_free_pattern(pattern);
      async_publish_score_failure(
          s, request_id,
          score_allocation_failed
              ? "matcher could not allocate scoring scratch"
              : "matcher could not allocate result storage");
      free(flat);
      free(window_values);
      free(merge_values);
      free(membership.idx);
      free(batches);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      shared_idx_release(refine_idx);
      free(growth_top);
      continue;
    }
    free(batches);
    batches = NULL;
    free(window_values);
    window_values = NULL;
    free(merge_values);
    merge_values = NULL;

    if (incremental_growth &&
        growth_matched_count > SIZE_MAX - delta_matches) {
      free(membership.idx);
      if (pattern) fzf_free_pattern(pattern);
      async_publish_score_failure(
          s, request_id, "matcher result count overflowed");
      free(flat);
      free(growth_top);
      shared_idx_release(refine_idx);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }
    size_t matched_total = incremental_growth
                               ? growth_matched_count + delta_matches
                               : delta_matches;

    /* Membership is optional cache evidence.  It is authoritative only when
       the independent builder retained every matched index.  On overflow or
       allocation failure the builder discards its prefix, so an incomplete
       set can never be mistaken for refinement evidence. */
    bool membership_complete = membership.enabled &&
                               membership.count == matched_total;
    uint32_t *m_idx_buf = membership_complete ? membership.idx : NULL;
    if (membership_complete)
      membership.idx = NULL;
    else
      async_membership_disable(&membership);

    size_t ranked_count = pos;
    if (!bounded_full && incremental_growth && growth_top_count) {
      if (!delta_matches) {
        free(flat);
        flat = growth_top;
        growth_top = NULL;
        ranked_count = growth_top_count;
      } else if (growth_top_count <= SIZE_MAX - delta_matches) {
        size_t merged_count = growth_top_count + delta_matches;
        ScoredStr *merged = malloc(merged_count * sizeof *merged);
        if (merged) {
          memcpy(merged, growth_top,
                 growth_top_count * sizeof *merged);
          memcpy(merged + growth_top_count, flat,
                 delta_matches * sizeof *merged);
          free(flat);
          flat = merged;
          ranked_count = merged_count;
        } else {
          free(m_idx_buf);
          if (pattern) fzf_free_pattern(pattern);
          async_publish_score_failure(
              s, request_id, "matcher could not merge incremental results");
          free(flat);
          free(growth_top);
          shared_idx_release(refine_idx);
          free(batches);
          free(filter);
          batch_cache_release_query(&s->batch_cache, source_query);
          batch_cache_release_query(&s->batch_cache, target_query);
          continue;
        }
      }
    }
    free(growth_top);
    growth_top = NULL;

    /* Unlimited full-mode sorts the complete set on an ordinary run, or all
       cached results plus delta matches on an incremental run.  Positive
       limits were already reduced by the per-window top-K merge above. */
    if (has_pattern && !filter_only_mode && !bounded_full &&
        ranked_count > 1 &&
        !counting_sort_scored_abortable(
            flat, ranked_count, &s->score_abort)) {
      free(m_idx_buf);
      shared_idx_release(refine_idx);
      if (pattern) fzf_free_pattern(pattern);
      async_clear_current_request(s, request_id);
      free(flat);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }

    size_t emit = (limit && limit < ranked_count) ? limit : ranked_count;

    /* Filter-only display ordering: re-score and sort the emit window so
       the user sees best-of-emit first (not pool order).  Cost is
       bounded by `emit' (normally the positive result limit), not pool size.
       Refinement membership, when it fits its independent byte cap, was
       collected from every match before this ranking step. */
    if (filter_only_mode && has_pattern && flat && emit > 1) {
      fzf_slab_t *rank_slab = fzf_make_default_slab();
      bool rank_allocation_failed = false;
      bool rank_aborted = false;
      for (size_t i = 0; i < emit; i++) {
        if ((i & 0xFF) == 0 && async_stop_requested(&s->score_abort)) {
          rank_aborted = true;
          break;
        }
        flat[i].score = fzf_get_score(flat[i].str, pattern, rank_slab);
        if (fzf_allocation_failed()) {
          rank_allocation_failed = true;
          break;
        }
      }
      if (rank_slab) fzf_free_slab(rank_slab);
      if (rank_allocation_failed) {
        free(m_idx_buf);
        shared_idx_release(refine_idx);
        if (pattern) fzf_free_pattern(pattern);
        async_publish_score_failure(
            s, request_id, "matcher could not allocate ranking scratch");
        free(flat);
        free(batches);
        free(filter);
        batch_cache_release_query(&s->batch_cache, source_query);
        batch_cache_release_query(&s->batch_cache, target_query);
        continue;
      }
      if (rank_aborted || !counting_sort_scored_abortable(
                              flat, emit, &s->score_abort)) {
        free(m_idx_buf);
        shared_idx_release(refine_idx);
        if (pattern) fzf_free_pattern(pattern);
        async_clear_current_request(s, request_id);
        free(flat);
        free(filter);
        batch_cache_release_query(&s->batch_cache, source_query);
        batch_cache_release_query(&s->batch_cache, target_query);
        continue;
      }
    }
    if (pattern) fzf_free_pattern(pattern);

    /* Only the emitted prefix escapes this request.  Release excess geometric
       capacity (and the un-emitted ranked tail) before publishing a long-lived
       session result.  A failed shrinking realloc leaves the valid allocation
       and its contents unchanged. */
    if (flat) {
      if (emit == 0) {
        free(flat);
        flat = NULL;
      } else {
        ScoredStr *trimmed = realloc(flat, emit * sizeof *trimmed);
        if (trimmed) flat = trimmed;
      }
    }

    /* Cache the result.  pool_gen = count (the pool size we actually scored).
       For refine runs, count may be > refine_delta_from, so the new entry
       supersedes the old one as a refinement source for the same query. */
    cache_insert_for_request_abortable(
        &s->cache, filter, count, case_mode, fuzzy,
        filter_only_mode, flat, emit, matched_total,
        m_idx_buf, m_idx_buf ? matched_total : 0, &s->score_abort);
    free(m_idx_buf);
    shared_idx_release(refine_idx);
    refine_idx = NULL;

    if (async_stop_requested(&s->score_abort)) {
      async_clear_current_request(s, request_id);
      free(flat);
      free(filter);
      batch_cache_release_query(&s->batch_cache, source_query);
      batch_cache_release_query(&s->batch_cache, target_query);
      continue;
    }

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
      s->last_filtered = matched_total;
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
            filter, matched_total, count, emit, reused_batches, scan_count);
    (void)reused_batches;
    (void)scan_count;

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

  /* A settled session re-submitted with the same query used to get a
     fresh request id and a snapshot-generation bump on every tick, so
     generation-driven pollers never went quiet.  When the published
     result already answers this exact request at the current pool
     boundary, return its id unchanged and publish nothing. */
  pthread_mutex_lock(&s->score_res_mu);
  uint64_t settled_id = s->score_result_id;
  bool result_matches =
      settled_id != 0 &&
      s->score_result_filter &&
      strcmp(s->score_result_filter, filter) == 0 &&
      s->score_result_limit == limit &&
      s->score_result_case_mode == case_mode &&
      s->score_result_fuzzy == fuzzy &&
      s->score_result_filter_only == requested_filter_only &&
      s->score_result_pool_gen == current_pool &&
      s->score_error_id != settled_id;
  pthread_mutex_unlock(&s->score_res_mu);
  if (result_matches) {
    pthread_mutex_lock(&s->score_req_mu);
    bool settled = s->score_req_id == 0 && !s->score_current_filter &&
                   s->score_latest_id == settled_id;
    pthread_mutex_unlock(&s->score_req_mu);
    if (settled) {
      free(filter);
      return settled_id;
    }
  }

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
  if (!latest_filter) {
    free(cached_top);
    shared_idx_release(cached_m_idx);
    free(filter);
    return 0;
  }

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
  if (current_changed)
    async_worker_pool_wake(atomic_load_explicit(
        &s->worker_pool, memory_order_acquire));

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
    /* CURRENT_POOL was sampled before the cache lookup, which mallocs
       and copies the cached top-K, so the window to here is wide.  A
       growth retry racing this submit saw score_latest_id already
       bumped and dropped its event with the flag consumed.  Re-check
       the live pool and re-arm so the published boundary cannot
       silently lag it. */
    pthread_mutex_lock(&s->mu);
    size_t live_pool = s->count;
    pthread_mutex_unlock(&s->mu);
    if (live_pool != current_pool)
      async_notify_candidate_growth(s);
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

/* Signal `(wrong-type-argument natnump VALUE)'. */
static void async_signal_not_natnum(emacs_env *env, emacs_value value) {
  emacs_value data_args[] = { env->intern(env, "natnump"), value };
  env->non_local_exit_signal(env, Qwrong_type_argument,
                             env->funcall(env, Flist, 2, data_args));
}

/* Submit one immutable query request from Emacs.  Identical queued or running
   work is reused so compatibility polling cannot create a duplicate scoring
   pass. */
static uint64_t async_submit_request(emacs_env *env, AsyncSession *s,
                                     char *filter, size_t filter_byte_len,
                                     size_t limit) {
  if (!filter) return 0;
  if (memchr(filter, '\0', filter_byte_len) != NULL) {
    free(filter);
    async_signal_error(
        env, "fzf-native: embedded NUL in query is not supported");
    return 0;
  }
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

#ifdef FZF_NATIVE_CTEST
static _Atomic bool async_test_fail_result_copy_allocation;
#endif

static void async_free_public_result(ScoredStr *result, size_t count) {
  if (!result) return;
  for (size_t i = 0; i < count; i++)
    free(result[i].str);
  free(result);
}

static ScoredStr *async_copy_public_result(
    AsyncSession *s, bool copy_candidates,
    size_t *out_count, AsyncResultObservation *out_result,
    char **out_filter, size_t *out_limit,
    fzf_case_types *out_case_mode, bool *out_fuzzy,
    bool *out_filter_only,
    uint64_t *out_snapshot_generation,
    size_t *out_progress_completed, size_t *out_progress_total,
    uint64_t *out_error_id, char **out_error,
    size_t *out_filtered, size_t *out_total,
    bool *out_allocation_failed) {
  pthread_mutex_lock(&s->score_res_mu);
  size_t count = s->score_count;
  ScoredStr *copy = NULL;
  bool allocation_failed = false;
  if (copy_candidates && count) {
    bool force_failure = false;
#ifdef FZF_NATIVE_CTEST
    force_failure = atomic_exchange_explicit(
        &async_test_fail_result_copy_allocation, false,
        memory_order_acq_rel);
#endif
    if (!force_failure && count <= SIZE_MAX / sizeof *copy)
      copy = calloc(count, sizeof *copy);
    if (copy && s->score_results) {
      for (size_t i = 0; i < count; i++) {
        copy[i].score = s->score_results[i].score;
        copy[i].idx = s->score_results[i].idx;
        if (s->score_results[i].str)
          copy[i].str = strdup(s->score_results[i].str);
        if (!copy[i].str) {
          allocation_failed = true;
          break;
        }
      }
    } else {
      allocation_failed = true;
    }
    if (allocation_failed) {
      async_free_public_result(copy, count);
      copy = NULL;
      count = 0;
    }
  }
  char *filter = s->score_result_filter
                     ? strdup(s->score_result_filter)
                     : NULL;
  if (s->score_result_filter && !filter) allocation_failed = true;
  *out_count = count;
  out_result->request_id = s->score_result_id;
  out_result->pool_generation = s->score_result_pool_gen;
  *out_filter = filter;
  *out_limit = s->score_result_limit;
  *out_case_mode = s->score_result_case_mode;
  *out_fuzzy = s->score_result_fuzzy;
  *out_filter_only = s->score_result_filter_only;
  *out_snapshot_generation = s->score_snapshot_generation;
  *out_progress_completed = s->score_result_progress_completed;
  *out_progress_total = s->score_result_progress_total;
  *out_error_id = s->score_error_id;
  *out_error = s->score_error ? strdup(s->score_error) : NULL;
  if (s->score_error && !*out_error) allocation_failed = true;
  *out_filtered = s->last_filtered;
  *out_total = s->last_total;
  *out_allocation_failed = allocation_failed;
  pthread_mutex_unlock(&s->score_res_mu);
  return copy;
}

static bool async_bytes_are_valid_utf8(const char *bytes, size_t length) {
  size_t offset = 0;
  while (offset < length) {
    utf8proc_int32_t codepoint = 0;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)bytes + offset,
        (utf8proc_ssize_t)(length - offset), &codepoint);
    if (width <= 0) return false;
    offset += (size_t)width;
  }
  return true;
}

/* Prefer an ordinary multibyte Lisp string for valid UTF-8, but retain an
   exact unibyte representation when a producer row or query contains raw
   bytes.  Decide from the bytes before calling Emacs: make_string can also
   signal memory-full or another conversion failure, and such a signal must
   remain pending rather than being mistaken for invalid UTF-8 and cleared.
   The matcher deliberately accepts raw bytes (Unix pathnames can contain
   them), so silently dropping the row at the public ABI made :filtered
   disagree with :candidates and made raw queries unreadable. */
static emacs_value async_make_lisp_string_lossless(
    emacs_env *env, const char *bytes, size_t length) {
  if (length > PTRDIFF_MAX) {
    async_signal_error(env, "fzf-native: string exceeds Emacs size limit");
    return Qnil;
  }
  return async_bytes_are_valid_utf8(bytes, length)
             ? env->make_string(env, bytes, (ptrdiff_t)length)
             : env->make_unibyte_string(env, bytes, (ptrdiff_t)length);
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

  /* `async_copy_public_result' deep-copies every SNAP string before this
     function can dispatch Lisp.  User Lisp reached through `cons', a
     highlight hook, or an advised primitive may stop the session and free
     its arena, but the snapshot and pending highlight data remain owned by
     this call.  Collect highlight positions before invoking the hook so the
     hook cannot invalidate later position work either. */
  struct pending_hl { char *str; fzf_position_t *pos; emacs_value val; };
  struct pending_hl *pending = NULL;
  size_t pending_count = 0;
  bool hook_wanted = hl_pattern && !env->eq(env, hl_hook, Qnil);
  if (hook_wanted) {
    size_t cap = count < hl_cap ? count : hl_cap;
    if (cap) pending = malloc(cap * sizeof *pending);
  }

  emacs_value result = Qnil;
  for (size_t i = count; i-- > 0;) {
    emacs_value str = async_make_lisp_string_lossless(
        env, snap[i].str, strlen(snap[i].str));
    enum emacs_funcall_exit status = env->non_local_exit_check(env);
    if (status != emacs_funcall_exit_return) {
      break;
    }

    if (pending && i < hl_cap) {
      char *copy = strdup(snap[i].str);
      if (copy) {
        pending[pending_count].str = copy;
        pending[pending_count].pos = fzf_get_positions(snap[i].str,
                                                       hl_pattern, hl_slab);
        if (fzf_allocation_failed()) {
          free(copy);
          async_signal_error(
              env, "fzf-native: matcher could not allocate highlight positions");
          break;
        }
        pending[pending_count].val = str;
        pending_count++;
      }
    }

    result = env->funcall(env, Fcons, 2, (emacs_value[]){ str, result });
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      break;
  }

  for (size_t j = 0; j < pending_count; j++) {
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
      dispatch_highlight_runs(env, pending[j].str, pending[j].pos,
                              pending[j].val, hl_hook, &hl_scratch);
    fzf_free_positions(pending[j].pos);
    free(pending[j].str);
  }
  free(pending);

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
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (!s) return Qnil;

  struct Str filter;
  if (!async_copy_lisp_string(env, args[1], "query", &filter)) return Qnil;

  size_t limit = 0;
  if (nargs > 2 && !env->eq(env, args[2], Qnil)) {
    intmax_t extracted = env->extract_integer(env, args[2]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
      free(filter.b);
      return Qnil;
    }
    if (extracted < 0) {
      free(filter.b);
      async_signal_not_natnum(env, args[2]);
      return Qnil;
    }
    limit = (size_t)extracted;
  }

  uint64_t request_id = async_submit_request(
      env, s, filter.b, filter.len, limit);
  if (!request_id) {
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
      async_signal_error(env, "fzf-native-async-candidates failed");
    return Qnil;
  }

  size_t rcount = 0, result_limit = 0;
  size_t progress_completed = 0, progress_total = 0;
  size_t filtered = 0, total = 0;
  uint64_t error_id = 0;
  AsyncResultObservation result_observation = {0};
  uint64_t snapshot_generation = 0;
  char *result_filter = NULL, *score_error = NULL;
  fzf_case_types result_case_mode = CaseSmart;
  bool result_fuzzy = true, result_filter_only = false;
  bool copy_failed = false;
  ScoredStr *snap = async_copy_public_result(
      s, true, &rcount, &result_observation, &result_filter, &result_limit,
      &result_case_mode, &result_fuzzy, &result_filter_only,
      &snapshot_generation,
      &progress_completed, &progress_total, &error_id, &score_error,
      &filtered, &total, &copy_failed);
  (void)result_observation;
  (void)result_limit;
  (void)result_filter_only;
  (void)snapshot_generation;
  (void)progress_completed;
  (void)progress_total;
  (void)error_id;
  (void)filtered;
  (void)total;

  if (copy_failed) {
    free(result_filter);
    free(score_error);
    async_free_public_result(snap, rcount);
    async_signal_error(
        env, "fzf-native: could not copy asynchronous results");
    return Qnil;
  }

  emacs_value result = async_build_candidate_list(
      env, snap, rcount, result_filter, result_case_mode, result_fuzzy);
  free(result_filter);
  free(score_error);
  async_free_public_result(snap, rcount);
  return result;
}

enum AsyncRequestState {
  AsyncRequestIdle,
  AsyncRequestQueued,
  AsyncRequestRunning,
  AsyncRequestComplete,
  AsyncRequestFailed,
  AsyncRequestCancelled,
  AsyncRequestSuperseded,
  AsyncRequestUnknown,
};

static enum AsyncRequestState async_request_state(
    uint64_t request_id, uint64_t latest_id, uint64_t queued_id,
    uint64_t running_id, uint64_t result_id, uint64_t failed_id) {
  if (request_id == 0) return AsyncRequestIdle;
  if (request_id == queued_id) return AsyncRequestQueued;
  if (request_id == running_id) return AsyncRequestRunning;
  if (request_id == failed_id) return AsyncRequestFailed;
  if (request_id == result_id) return AsyncRequestComplete;
  /* Only the latest request has retained execution state.  An older ID may
     have completed before a newer submission or may have been aborted; call
     it superseded rather than inventing a cancellation history we do not
     retain. */
  if (request_id <= latest_id) return AsyncRequestSuperseded;
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
    case AsyncRequestSuperseded: return "superseded";
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
  uint64_t error = atomic_load_explicit(
      &s->producer_error, memory_order_acquire);
  enum AsyncProducerErrorKind kind =
      async_unpack_producer_error_kind(error);
  int number = async_unpack_producer_error_number(error);
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
    case AsyncProducerErrorInvalidData:
      snprintf(buffer, capacity, "producer output contains a NUL byte");
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

static bool async_snapshot_is_stale(uint64_t requested_id,
                                    uint64_t result_id,
                                    size_t result_pool_gen,
                                    size_t current_pool,
                                    bool reader_done) {
  return requested_id != result_id ||
         (result_id != 0 && result_pool_gen != current_pool) ||
         (current_pool == 0 && !reader_done);
}

/* Capture the exact result/pool pair that `async_snapshot_value' serializes.
   Keeping the named observations together prevents positional scalar wiring
   from silently substituting the completed result boundary for the live pool
   boundary.  C tests exercise this same production capture routine. */
static AsyncSnapshotState async_capture_snapshot_state(
    AsyncSession *s, uint64_t requested_id,
    AsyncResultObservation result) {
  AsyncSnapshotState state = {
      .result = result,
      .pool = async_observe_pool(s),
  };
  state.stale = async_snapshot_is_stale(
      requested_id, state.result.request_id,
      state.result.pool_generation, state.pool.count,
      state.pool.reader_done);
  return state;
}

static emacs_value async_snapshot_value(emacs_env *env, AsyncSession *s,
                                        uint64_t requested_id,
                                        bool include_candidates) {
  /* Hold request ownership stable through the result copy.  Successful and
     failed publication take the same request -> result lock order, so the
     returned state and snapshot generation describe one atomic transition
     instead of request-before/result-after halves from different states. */
  pthread_mutex_lock(&s->score_req_mu);
  uint64_t latest_id = s->score_latest_id;
  uint64_t queued_id = s->score_req_id;
  uint64_t running_id = s->score_current_id;
  if (requested_id == 0) requested_id = latest_id;

  size_t result_count = 0, result_limit = 0;
  size_t progress_completed = 0, progress_total = 0;
  size_t filtered = 0, total = 0;
  uint64_t error_id = 0;
  AsyncResultObservation result_observation = {0};
  uint64_t snapshot_generation = 0;
  char *result_filter = NULL, *score_error = NULL;
  fzf_case_types result_case_mode = CaseSmart;
  bool result_fuzzy = true, result_filter_only = false;
  bool copy_failed = false;
  ScoredStr *result_copy = async_copy_public_result(
      s, include_candidates, &result_count, &result_observation,
      &result_filter, &result_limit,
      &result_case_mode, &result_fuzzy, &result_filter_only,
      &snapshot_generation,
      &progress_completed, &progress_total, &error_id, &score_error,
      &filtered, &total, &copy_failed);
  pthread_mutex_unlock(&s->score_req_mu);

  uint64_t result_id = result_observation.request_id;

  if (copy_failed) {
    free(result_filter);
    free(score_error);
    async_free_public_result(result_copy, result_count);
    async_signal_error(
        env, "fzf-native: could not copy asynchronous snapshot");
    return Qnil;
  }

  AsyncSnapshotState snapshot_state = async_capture_snapshot_state(
      s, requested_id, result_observation);
  size_t result_pool_gen = snapshot_state.result.pool_generation;
  size_t current_pool = snapshot_state.pool.count;
  bool reader_done = snapshot_state.pool.reader_done;

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
  size_t result_cache_entries = 0, result_cache_bytes = 0;
  uint64_t result_cache_evictions = 0;
  cache_stats(&s->cache, &result_cache_entries, &result_cache_bytes,
              &result_cache_evictions);
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
  /* No `s' access below this point: the highlight hook inside the
     candidate list build can run user Lisp that stops the session and
     frees it. */
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
  plist = async_plist_put(env, plist, ":cache-evictions",
                          env->make_integer(
                              env, (intmax_t)result_cache_evictions));
  plist = async_plist_put(env, plist, ":cache-bytes",
                          env->make_integer(
                              env, (intmax_t)result_cache_bytes));
  plist = async_plist_put(env, plist, ":cache-entries",
                          env->make_integer(
                              env, (intmax_t)result_cache_entries));
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
                          reader_done ? Qt : Qnil);
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
          ? async_make_lisp_string_lossless(
                env, result_filter, strlen(result_filter))
          : Qnil);
  /* Post-restart warmup: `result_pool_gen == current_pool == 0' passes
     the pool check trivially (nothing was scored), but the producer may
     still emit candidates that change the answer.  Mirror the guard in
     `fzf-native-async-result-fresh-p': an empty-pool result is not
     authoritative until the reader has observed EOF (or stop). */
  bool stale = snapshot_state.stale;
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
  async_free_public_result(result_copy, result_count);
  return plist;
}

/* fzf-native-async-submit HANDLE QUERY &optional LIMIT -> request ID */
static emacs_value
fzf_native_async_submit(emacs_env *env, ptrdiff_t nargs,
                        emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (!s) {
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
      async_signal_error(env, "fzf-native: session is stopped");
    return Qnil;
  }

  struct Str query;
  if (!async_copy_lisp_string(env, args[1], "query", &query)) return Qnil;

  size_t limit = 0;
  if (nargs > 2 && !env->eq(env, args[2], Qnil)) {
    intmax_t extracted = env->extract_integer(env, args[2]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
      free(query.b);
      return Qnil;
    }
    if (extracted < 0) {
      free(query.b);
      async_signal_not_natnum(env, args[2]);
      return Qnil;
    }
    limit = (size_t)extracted;
  }

  uint64_t request_id = async_submit_request(
      env, s, query.b, query.len, limit);
  if (!request_id) {
    if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
      async_signal_error(env, "fzf-native-async-submit failed");
    return Qnil;
  }
  return env->make_integer(env, (intmax_t)request_id);
}

/* fzf-native-async-snapshot HANDLE &optional REQUEST-ID -> plist */
static emacs_value
fzf_native_async_snapshot(emacs_env *env, ptrdiff_t nargs,
                          emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (!s) return Qnil;
  uint64_t request_id = 0;
  if (nargs > 1 && !env->eq(env, args[1], Qnil)) {
    intmax_t extracted = env->extract_integer(env, args[1]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      return Qnil;
    if (extracted < 0) {
      async_signal_not_natnum(env, args[1]);
      return Qnil;
    }
    request_id = (uint64_t)extracted;
  }
  return async_snapshot_value(env, s, request_id, true);
}

/* fzf-native-async-status HANDLE &optional REQUEST-ID -> plist */
static emacs_value
fzf_native_async_status(emacs_env *env, ptrdiff_t nargs,
                        emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (!s) return Qnil;
  uint64_t request_id = 0;
  if (nargs > 1 && !env->eq(env, args[1], Qnil)) {
    intmax_t extracted = env->extract_integer(env, args[1]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
      return Qnil;
    if (extracted < 0) {
      async_signal_not_natnum(env, args[1]);
      return Qnil;
    }
    request_id = (uint64_t)extracted;
  }
  return async_snapshot_value(env, s, request_id, false);
}

/* fzf-native-async-stats HANDLE -> (filtered . total) */
static emacs_value
fzf_native_async_stats(emacs_env *env, ptrdiff_t UNUSED(nargs),
                       emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
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
  AsyncSession *s = fzf_native_typed_user_ptr(
      env, args[0], async_session_destroy_async,
      "an fzf-native async session handle");
  if (!s) return Qnil;

  struct Str query;
  if (!async_copy_lisp_string(env, args[1], "query", &query)) return Qnil;

  AsyncPoolObservation pool = async_observe_pool(s);
  size_t cur_pool = pool.count;

  fzf_case_types case_mode = resolve_fzf_native_case_mode(env);
  bool fuzzy = resolve_fzf_native_fuzzy(env);
  size_t fo_unused_min = 0, fo_max_len = 0;
  bool fo_logic_and = resolve_filter_only_settings(
      env, &fo_unused_min, &fo_max_len);
  (void)fo_unused_min;
  size_t query_char_len = utf8_character_count(query.b, query.len);
  bool filter_only = decide_filter_only(
      s->filter_only_min_pool, fo_max_len, fo_logic_and,
      query_char_len, cur_pool);

  pthread_mutex_lock(&s->cache.mu);
  CacheEntry *e = cache_find_locked(&s->cache, query.b, case_mode, fuzzy);
  bool fresh = (e != NULL && e->pool_gen == cur_pool &&
                e->filter_only == filter_only);
  pthread_mutex_unlock(&s->cache.mu);

  /* A larger-limit or mode-changing request can refine an otherwise fresh
     cache entry.  During that work, the old top-K is not authoritative for
     the current request. */
  pthread_mutex_lock(&s->score_req_mu);
  bool same_query_in_flight =
      (s->score_req_filter &&
       strcmp(s->score_req_filter, query.b) == 0 &&
       s->score_req_case_mode == case_mode &&
       s->score_req_fuzzy == fuzzy) ||
      (s->score_current_filter &&
       strcmp(s->score_current_filter, query.b) == 0 &&
       s->score_current_case_mode == case_mode &&
       s->score_current_fuzzy == fuzzy);
  pthread_mutex_unlock(&s->score_req_mu);
  if (same_query_in_flight) fresh = false;

  /* Empty pool while the producer is still streaming: cache match is
     trivial (scoring 0 items), not authoritative. */
  if (fresh && cur_pool == 0 && !pool.reader_done) {
    fresh = false;
  }

  free(query.b);
  return fresh ? Qt : Qnil;
}

#endif /* APPLE || linux || FreeBSD */

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
static emacs_value
fzf_native_session_abi_version(emacs_env *env, ptrdiff_t UNUSED(nargs),
                               emacs_value UNUSED(args[]),
                               void *UNUSED(data)) {
  return env->make_integer(env, FZF_NATIVE_SESSION_ABI);
}
#endif

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

EXPORT
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
      env->intern(env, "fzf-native-session-abi-version"),
      env->make_function(env, 0, 0, fzf_native_session_abi_version,
                         "Return the interactive-session ABI version.\n\n"
                         "\\(fn)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-start"),
      env->make_function(env, 1, 2, fzf_native_async_start,
                         "Start async shell COMMAND; return a session handle.\n"
                         "Optional DIR sets the working directory (default: Emacs cwd).\n\n"
                         "Each stdout line is one candidate.  A line with a NUL byte\n"
                         "fails the producer session.\n\n"
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
                         "waits for scoring and does not build an Emacs candidate list.\n"
                         "Signals instead of returning nil on failure: `error' if HANDLE\n"
                         "is stopped, `wrong-type-argument' if QUERY is not an encodable\n"
                         "string or LIMIT is not a natural number.\n\n"
                         "\\(fn HANDLE QUERY &optional LIMIT)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-snapshot"),
      env->make_function(env, 1, 2, fzf_native_async_snapshot,
                         "Return a request-aware result plist for async HANDLE.\n"
                         "Optional REQUEST-ID selects the request to inspect; nil or 0\n"
                         "means the latest submitted request.  The plist retains the last\n"
                         "completed candidates while newer work is queued or running.\n"
                         "nil if HANDLE is stopped.\n"
                         "\n"
                         "About the inspected request:\n"
                         "  :request-id  the id this plist answers about\n"
                         "  :state       one of the symbols idle, queued, running,\n"
                         "               complete, failed, superseded, unknown\n"
                         "  :stale       t unless the retained result belongs to this\n"
                         "               request AND covers the current pool.  Finality is\n"
                         "               (and (eq :state 'complete) (not :stale)).\n"
                         "  :progress-completed :progress-total  scoring progress\n"
                         "\n"
                         "About the retained (last published) result:\n"
                         "  :candidates  matched strings, best first (snapshot only)\n"
                         "  :filtered :total  match count / pool size for a counts overlay\n"
                         "  :result-request-id :result-pool-generation  which request\n"
                         "               produced it, at what pool boundary\n"
                         "  :query :limit :case-mode :fuzzy :filter-only  the options it\n"
                         "               was computed with.  These describe the retained\n"
                         "               result, NOT necessarily REQUEST-ID's options;\n"
                         "               when :stale, they belong to another request.\n"
                         "  :error :failed-request-id  scoring error, if any\n"
                         "\n"
                         "About the session:\n"
                         "  :latest-request-id  most recently submitted id\n"
                         "  :pool-generation    current candidate pool size\n"
                         "  :snapshot-generation  bumps per result or producer terminal event\n"
                         "  :reader-done        producer stream fully consumed\n"
                         "  :producer-state :producer-exit-status :producer-error\n"
                         "               check these before treating an empty final\n"
                         "               result as \"no matches\" -- a failed producer\n"
                         "               also completes with :error nil\n"
                         "  :cache-entries :cache-bytes :cache-evictions\n"
                         "               whole-result cache telemetry\n"
                         "  :batch-cache-entries :batch-cache-bytes :batch-cache-hits\n"
                         "  :batch-cache-misses :batch-cache-evictions  cache telemetry\n\n"
                         "\\(fn HANDLE &optional REQUEST-ID)", NULL),
    });
  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-async-status"),
      env->make_function(env, 1, 2, fzf_native_async_status,
                         "Return request and producer status for async HANDLE.\n"
                         "This is the metadata-only counterpart to\n"
                         "`fzf-native-async-snapshot': the same plist minus\n"
                         ":candidates, so polling never pays the candidate\n"
                         "list build.  Optional REQUEST-ID selects the request\n"
                         "to inspect; without it, inspect the latest one.\n\n"
                         "\\(fn HANDLE &optional REQUEST-ID)", NULL),
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
  Fmultibyte_string_p = env->make_global_ref(
      env, env->intern(env, "multibyte-string-p"));
  Fsetcar = env->make_global_ref(env, env->intern(env, "setcar"));
  Faset = env->make_global_ref(env, env->intern(env, "aset"));
  Fsymbol_value = env->make_global_ref(env, env->intern(env, "symbol-value"));
  Qsym_case_mode            = env->make_global_ref(env, env->intern(env, "fzf-native-case-mode"));
  Qsym_fuzzy                = env->make_global_ref(env, env->intern(env, "fzf-native-fuzzy"));
  Qsym_batch_highlight      = env->make_global_ref(env, env->intern(env, "fzf-native-batch-highlight"));
  Qsym_async_highlight      = env->make_global_ref(env, env->intern(env, "fzf-native-async-highlight"));
  Qsym_max_line_length      = env->make_global_ref(env, env->intern(env, "fzf-native-max-line-length"));
  Qsym_async_cache_size     = env->make_global_ref(env, env->intern(env, "fzf-native-async-cache-size"));
  Qsym_async_cache_bytes    = env->make_global_ref(
      env, env->intern(env, "fzf-native-async-cache-bytes"));
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
