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

#define ASYNC_INIT_CAP 4096
#define ASYNC_LINE_MAX 8192

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

typedef struct {
  pthread_t     reader;
  pid_t         pid;
  FILE         *fp;
  volatile bool stop;

  pthread_mutex_t mu;
  char          **cands;
  size_t          count;
  size_t          cap;
  _Atomic int     gen;

  size_t          last_filtered;   /* candidates matching last filter */
  size_t          last_total;      /* total candidates at last call */
} AsyncSession;

static void *async_reader(void *arg) {
  AsyncSession *s = arg;
  fzf_log("async_reader START: pid=%d\n", (int)s->pid);
  char line[ASYNC_LINE_MAX];
  while (!s->stop && s->fp && fgets(line, sizeof line, s->fp)) {
    size_t len = strlen(line);
    while (len && (line[len - 1] == '\n' || line[len - 1] == '\r'))
      line[--len] = '\0';
    len = async_strip_ansi(line, len);
    if (!len) continue;

    char *dup = strdup(line);
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

static void async_session_destroy(void *ptr) {
  AsyncSession *s = ptr;
  if (!s) return;
  fzf_log("async_session_destroy: pid=%d count=%zu\n", (int)s->pid, s->count);
  s->stop = true;
  /* SIGTERM → child exits → pipe EOF → fgets returns NULL → reader exits */
  if (s->pid > 0) kill(s->pid, SIGTERM);
  pthread_join(s->reader, NULL);
  if (s->fp)      { fclose(s->fp); s->fp = NULL; }
  if (s->pid > 0) { waitpid(s->pid, NULL, 0); s->pid = -1; }
  pthread_mutex_lock(&s->mu);
  for (size_t i = 0; i < s->count; i++) free(s->cands[i]);
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
  s->cap   = ASYNC_INIT_CAP;
  s->cands = malloc(s->cap * sizeof *s->cands);
  pthread_mutex_init(&s->mu, NULL);
  atomic_store(&s->gen, 0);

  if (!s->fp || !s->cands ||
      pthread_create(&s->reader, NULL, async_reader, s) != 0) {
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

typedef struct { char *str; int score; } ScoredStr;
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
};

static void *async_scoring_worker(void *ptr) {
  struct AsyncScoringShared *shared = ptr;
  fzf_slab_t    *slab         = fzf_make_default_slab();
  fzf_pattern_t *pattern      = shared->pattern;

  ssize_t bi;
  while ((bi = atomic_fetch_sub_explicit(&shared->remaining, 1,
                                         memory_order_seq_cst) - 1) >= 0) {
    struct AsyncScoringBatch *batch = shared->batches + bi;
    unsigned n = 0;
    for (unsigned i = 0; i < batch->len; i++) {
      int sc = pattern ? fzf_get_score(batch->xs[i].str, pattern, slab) : 1;
      if (!pattern || sc > 0) {
        batch->xs[n]         = batch->xs[i];
        batch->xs[n++].score = sc;
      }
    }
    batch->len = n;
  }

  fzf_free_slab(slab);
  return NULL;
}

/* fzf-native-async-candidates HANDLE FILTER &optional LIMIT -> list of strings, scored */
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

  fzf_log("async_candidates START: filter='%s' gen=%d\n",
          filter, (int)atomic_load_explicit(&s->gen, memory_order_relaxed));

  /* Snapshot candidate pointers under lock.
     Strings are strdup'd and never freed until session destroy,
     so pointers remain valid after we release the lock. */
  pthread_mutex_lock(&s->mu);
  size_t  count = s->count;
  char  **snap  = count ? malloc(count * sizeof *snap) : NULL;
  if (snap) memcpy(snap, s->cands, count * sizeof *snap);
  pthread_mutex_unlock(&s->mu);

  if (count && !snap) { free(filter); return Qnil; }

  /* Distribute snap into BATCH_SIZE batches (same pattern as fzf_native_score_all) */
  struct AsyncScoringBatch *batches = NULL;
  size_t bi = 0, cap = 0;
  for (size_t i = 0; i < count; i++) {
    if (!batches || (batches[bi].len >= BATCH_SIZE && ++bi >= cap)) {
      cap = cap ? cap * 2 : 1;
      struct AsyncScoringBatch *nb = realloc(batches, cap * sizeof *nb);
      if (!nb) { free(snap); free(filter); free(batches); return Qnil; }
      for (size_t k = bi; k < cap; k++) nb[k].len = 0;
      batches = nb;
    }
    batches[bi].xs[batches[bi].len].str   = snap[i];
    batches[bi].xs[batches[bi].len].score = 0;
    batches[bi].len++;
  }

  unsigned num_batches = count ? (unsigned)(bi + 1) : 0;
  unsigned max_workers = (unsigned)sysconf(_SC_NPROCESSORS_ONLN);

  fzf_pattern_t *pattern = (flen > 1) ? fzf_parse_pattern(CaseIgnore, false, filter, true) : NULL;
  struct AsyncScoringShared shared = {
    .pattern   = pattern,
    .batches   = batches,
    .remaining = num_batches,
  };

  pthread_t *threads    = malloc(max_workers * sizeof *threads);
  unsigned  num_workers = 0;
  if (threads && num_batches) {
    for (; num_workers < MIN(max_workers, num_batches); num_workers++)
      pthread_create(threads + num_workers, NULL, async_scoring_worker, &shared);
  }
  for (unsigned i = 0; i < num_workers; i++)
    pthread_join(threads[i], NULL);
  free(threads);

  /* Compact all surviving entries into a flat array for sorting */
  size_t total = 0;
  for (unsigned i = 0; i < num_batches; i++) total += batches[i].len;

  /* Optional limit: how many candidates to return to Elisp. */
  size_t limit = 0;
  if (nargs > 2 && !env->eq(env, args[2], Qnil))
    limit = (size_t)env->extract_integer(env, args[2]);

  ScoredStr *flat = total ? malloc(total * sizeof *flat) : NULL;
  size_t pos = 0;
  if (flat) {
    for (unsigned i = 0; i < num_batches; i++) {
      struct AsyncScoringBatch *b = batches + i;
      for (unsigned j = 0; j < b->len; j++)
        flat[pos++] = b->xs[j];
    }
    if (pattern && pos > 1)
      counting_sort_scored(flat, pos);
  }

  /* Record stats (full filtered count, not the capped emit count). */
  s->last_filtered = pos;
  s->last_total    = count;

  /* Emit at most `limit' candidates; 0 means no cap. */
  size_t emit = (limit > 0 && limit < pos) ? limit : pos;

  /* Build Emacs list on the main thread */
  emacs_value result = Qnil;
  for (size_t i = emit; i-- > 0;) {
    emacs_value str = env->make_string(env, flat[i].str,
                                       (ptrdiff_t)strlen(flat[i].str));
    /* make_string signals if the shell emitted non-UTF-8 bytes (e.g. a
       Latin-1 filename on a non-UTF-8 filesystem).  Skip the offending
       candidate rather than truncating the rest of the list. */
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
      env->non_local_exit_clear(env);
      continue;
    }
    result = env->funcall(env, Fcons, 2, (emacs_value[]){ str, result });
  }

  fzf_log("async_candidates DONE: filter='%s' filtered=%zu total=%zu emit=%zu\n",
          filter, s->last_filtered, s->last_total, emit);

  if (pattern) fzf_free_pattern(pattern);
  free(flat);
  free(snap);
  free(batches);
  free(filter);
  return result;
}

/* fzf-native-async-stats HANDLE -> (filtered . total) */
static emacs_value
fzf_native_async_stats(emacs_env *env, ptrdiff_t UNUSED(nargs),
                       emacs_value args[], void *UNUSED(data)) {
  AsyncSession *s = env->get_user_ptr(env, args[0]);
  if (!s) return Qnil;
  return env->funcall(env, Fcons, 2, (emacs_value[]){
    env->make_integer(env, (intmax_t)s->last_filtered),
    env->make_integer(env, (intmax_t)s->last_total),
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
