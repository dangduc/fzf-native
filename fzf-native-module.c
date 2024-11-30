#include <ctype.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "emacs-module.h"
#include "fzf.h"
#include <stdatomic.h>
#include <stdio.h>

// Windows pthread.h conflicts with the system Time.
// e.g. 'timespec': 'struct' type redefinition
#ifdef _WIN32
#define _TIMESPEC_DEFINED
#endif
#include <pthread.h>

#if defined(__APPLE__) || defined(__linux__)
// for sysconf(_SC_NPROCESSORS_ONLN);
#include <unistd.h>
#endif

#ifdef _WIN32
#  define EXPORT __declspec(dllexport)
#else
#  define EXPORT
#endif

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
emacs_value Fhashtablep, Fmessage, Fvectorp, Fconsp, Fcdr, Fcar;
emacs_value Ffunctionp, Fsymbolp, Fsymbolname, Flength, Fnth, Fprinc, Freverse;
emacs_value Qcompletion_score, Fput_text_property, Qzero, Qone;


/** An Emacs string made accessible by copying. */
struct Str { char *b; size_t len; };

/** Module userdata that gets allocated once at initialization. */
struct Data {
  unsigned max_workers;
  pthread_t threads[];
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

// Deep copy function
struct Str copy_str(const struct Str *src) {
  struct Str dest = { NULL, 0 };

  // Check if the source string is valid
  if (src && src->b && src->len > 0) {
    // Allocate memory for the destination string
    dest.b = malloc(src->len + 1); // +1 for null terminator
    if (dest.b) {
      // Copy the string contents
      memcpy(dest.b, src->b, src->len + 1);
      dest.b[src->len] = '\0'; // Ensure null termination
      dest.len = src->len;
    }
  }

  return dest;
}

// Function to free the deep copy
void free_str(struct Str *str) {
  if (str && str->b) {
    free(str->b);
    str->b = NULL;
    str->len = 0;
  }
}

// Copied from https://github.com/axelf4/hotfuzz
/** Copies the Emacs string to make its contents accessible. */
static struct Str copy_emacs_string(emacs_env *env, struct Bump **bump, emacs_value value) {
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

struct Candidate {
  emacs_value value;
  union {
    struct Str s;
    int score;
  };
};

static int cmp_candidate(const void *a, const void *b) {
  // This way to get fzf sorted correctly with qsort.
  return ((struct Candidate *) b)->score - ((struct Candidate *) a)->score;
  /* return ((struct Candidate *) a)->score - ((struct Candidate *) b)->score; */
}

struct Batch {
  unsigned len;
  struct Candidate xs[BATCH_SIZE];
};

struct Shared {
  const struct Str query;
  struct Batch *const batches;
  _Atomic ssize_t remaining;
};

// Most of the threading lifted from https://github.com/axelf4/hotfuzz
static void *worker_routine(void *ptr) {
  /* printf("-----\nStarting Worker Routine\n-----\n"); */
  // Create a one-time use slab.
  fzf_slab_t *slab = fzf_make_default_slab();

  struct Shared *shared = ptr;
  struct Str shared_query = shared->query;
  // Create a deep copy of the original string.
  // fzf_parse_pattern mutilates the char* in Str so using the original string
  // is unsafe in a multithreaded context.
  struct Str query = copy_str(&shared_query);
  ssize_t batch_idx;

  // Atomic fetch-and-decrement for shared->remaining
  // --shared->remaining would return the decremented value whereas
  // atomic_fetch_sub_explicit returns the original value before decrement.
  // So, use batch_idx - 1 when handling the idx.
  while ((batch_idx = atomic_fetch_sub_explicit(&shared->remaining,
                                                1,
                                                memory_order_seq_cst) - 1) >= 0) {
    struct Batch *batch = shared->batches + batch_idx;
    unsigned n = 0;
    for (unsigned i = 0; i < batch->len; ++i) {
      struct Candidate x = batch->xs[i];
      /* fzf_case_mode enum : CaseSmart = 0, CaseIgnore, CaseRespect
       * normalize bool     : Always set to false because its not implemented yet.
       *                      This is reserved for future use
       * pattern char*      : Pattern you want to match. e.g. "src | lua !.c$
       * fuzzy bool         : Enable or disable fuzzy matching
       */
      fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, false, query.b, true);
      /* You can get the score/position for as many items as you want */
      int score = fzf_get_score(x.s.b, pattern, slab);
      if (score > 0) {
        /* printf("Str: %s # = %d | i = %d, batch->len = %d, batch_idx = %zd\n", */
        /*        x.s.b, score, i, batch->len, batch_idx); */
        x.score = score;
        batch->xs[n++] = x;
      }
      fzf_free_pattern(pattern);
    }
    batch->len = n;
  }

  free_str(&query);
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
  int success = false;
  emacs_value result = Qnil;

  // Collect all candidates
  struct Batch *batches = NULL;
  size_t batch_idx = 0, capacity;
  for (emacs_value list = args[0]; env->is_not_nil(env, list);
       list = env->funcall(env, Fcdr, 1, (emacs_value[]) { list })) {
    if (!batches || (batches[batch_idx].len >= BATCH_SIZE && ++batch_idx >= capacity)) {
      capacity = batches ? 2 * capacity : 1;
      struct Batch *new_batches;
      if (!(new_batches = realloc(batches, capacity * sizeof *batches))) goto err;
      batches = new_batches;
      for (size_t i = batch_idx; i < capacity; ++i) batches[i].len = 0;
    }

    emacs_value value = env->funcall(env, Fcar, 1, (emacs_value[]) { list });
    struct Batch *batch = batches + batch_idx;
    struct Candidate *x = batch->xs + batch->len++;
    if (!(x->s = copy_emacs_string(env, &bump, x->value = value)).b) goto err;
  }

  if (!batches) {
    return Qnil;
  }

  /* struct Str query = copy_emacs_string(env, &bump, QUERY); */
  struct Str query = copy_emacs_string(env, &bump, args[1]);
  if (!query.b) { goto err; }

  struct Shared shared = {
    .query = query,
    .batches = batches,
    .remaining = batch_idx + 1,
  };

  // Print the shared value.
  /* ssize_t value = atomic_load(&shared.remaining); */
  /* printf("shared Remaining: %zd\n", value); */
#if defined(__APPLE__) || defined(__linux__)
  // Set up max number of workers according to processor.
  // It's 8 on M1 Macbook.
  unsigned max_workers = sysconf(_SC_NPROCESSORS_ONLN);
#else
  unsigned max_workers = 4; // Random safe guess number.
#endif

  if (!(data = malloc(sizeof *data + max_workers * sizeof *data->threads))) {
    goto err;
  }
  *data = (struct Data) { max_workers };

  unsigned num_workers = 0;
  for (; num_workers < MIN(data->max_workers, batch_idx + 1); ++num_workers)
    if (pthread_create(data->threads + num_workers, NULL, worker_routine, &shared))
      // Join all workers in order to at least safely free memory
      goto err_join_threads;
  success = true;

err_join_threads:
  // Wait for all worker threads
  for (unsigned i = 0; i < num_workers; ++i) pthread_join(data->threads[i], NULL);
  if (!success) goto err;
  if (env->process_input(env) == emacs_process_input_quit) goto err;

  // Compact all batches
  size_t len = batches[0].len;
  struct Candidate *xs = batches[0].xs;
  for (struct Batch *b = batches + 1; b <= batches + batch_idx; ++b) {
    unsigned n = b->len;
    memmove(xs + len, b->xs, n * sizeof *b->xs);
    len += n;
  }
  qsort(xs, len, sizeof *xs, cmp_candidate); // Sort the completions

  for (size_t i = len; i-- > 0;) {
    /* printf("zero: %jd one: %jd score: %d", */
    /*        env->extract_integer(env, Qzero), */
    /*        env->extract_integer(env, Qone), */
    /*        xs[i].score); */
    /* e.g. (put-text-property 0 1 'completion-score score x) */
    env->funcall(env, Fput_text_property, 5,
                 (emacs_value[]) {
                   Qzero, Qone, Qcompletion_score,
                   env->make_integer(env, xs[i].score),
                   xs[i].value,
                 });

    result = env->funcall(env, Fcons, 2, (emacs_value[]) { xs[i].value, result });
  }

err:
  free(batches);
  bump_free(bump);
  free(data);

  if (!success) {
    env->non_local_exit_signal(env, env->intern(env, "error"), Qnil);
  }
  return result;
}

// fzf-native-score STR QUERY &optional SLAB
emacs_value fzf_native_score(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void UNUSED(*data_ptr)) {
  // Short-circuit if QUERY is empty.
  ptrdiff_t query_len;
  env->copy_string_contents(env, args[1], NULL, &query_len);
  if (query_len == /* solely null byte */ 1) {
    return Qlistofzero;
  }

  // Short-circuit if STR is empty.
  ptrdiff_t str_len;
  env->copy_string_contents(env, args[0], NULL, &str_len);
  if (str_len == /* solely null byte */ 1) {
    return Qlistofzero;
  }

  int success = false;
  struct Bump *bump = NULL;

  struct Str str = copy_emacs_string(env, &bump, args[0]);
  if (!str.b) { goto err; }

  struct Str query = copy_emacs_string(env, &bump, args[1]);
  if (!query.b) { goto err; }

  fzf_slab_t *slab;
  if (nargs > 2) {
    // Re-use SLAB argument.
    slab = env->get_user_ptr(env, args[2]);
  } else {
    // Create a one-time use slab.
    slab = fzf_make_default_slab();
  }

  /* fzf_case_mode enum : CaseSmart = 0, CaseIgnore, CaseRespect
   * normalize bool     : Always set to false because its not implemented yet.
   *                      This is reserved for future use
   * pattern char*      : Pattern you want to match. e.g. "src | lua !.c$
   * fuzzy bool         : Enable or disable fuzzy matching
   */
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseSmart, false, query.b, true);

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

  emacs_value result = env->funcall(env, Flist, offset + len, result_array);
  success = true;
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
  if (!success) {
    env->non_local_exit_signal(env, env->intern(env, "error"), Qnil);
  }

  return result;
}

void slab_finalize(void *object) {
  fzf_slab_t *slab = (fzf_slab_t *)object;
  fzf_free_slab(slab);
}

emacs_value fzf_native_make_default_slab(emacs_env UNUSED(*env),
                                         ptrdiff_t UNUSED(nargs),
                                         emacs_value UNUSED(args[]),
                                         void UNUSED(*data_ptr)) {
  fzf_slab_t *slab = fzf_make_default_slab();

  return env->make_user_ptr(env, slab_finalize, slab);
}

emacs_value fzf_native_make_slab(emacs_env UNUSED(*env),
                                 ptrdiff_t UNUSED(nargs),
                                 emacs_value UNUSED(args[]),
                                 void UNUSED(*data_ptr)) {
  size_t slab16Size = env->extract_integer(env, args[0]);
  size_t slab32Size = env->extract_integer(env, args[1]);

  fzf_slab_t *slab = fzf_make_slab((fzf_slab_config_t){slab16Size, slab32Size});

  return env->make_user_ptr(env, slab_finalize, slab);
}

int emacs_module_init(struct emacs_runtime *rt) {
  // Verify compatability with Emacs executable loading this module
  if ((size_t) rt->size < sizeof *rt)
    return 1;
  emacs_env *env = rt->get_environment(rt);
  if ((size_t) env->size < sizeof *env)
    return 2;

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

  Qlistofzero = env->make_global_ref(
    env, env->funcall(env, Fcons, 2,
                      (emacs_value[]){env->make_integer(env, 0), Qnil}));

  Qzero = env->make_global_ref(env, env->make_integer(env, 0));
  Qone = env->make_global_ref(env, env->make_integer(env, 1));

  return 0;
}
