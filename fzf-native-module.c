#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "emacs-module.h"
#include "fzf.h"

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

EXPORT
int plugin_is_GPL_compatible;

emacs_value Qnil, Qlistofzero, Qcons, Flist, Qt;
emacs_value Fhashtablep, Fmessage, Fvectorp, Fconsp, Ffunctionp, Fsymbolp, Fsymbolname, Flength, Fnth, Fprinc;


/** An Emacs string made accessible by copying. */
struct Str { char *b; size_t len; };

/** Module userdata that gets allocated once at initialization. */
struct Data {
  // NOLINTNEXTLINE: Ignore warning for unused declaration (clang).
  size_t placeholder; ///< C requires that a struct or union has at least one member.
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

// fzf-native-score-all COLLECTION QUERY &optional SLAB
emacs_value fzf_native_score_all(emacs_env *env,
                                 ptrdiff_t nargs,
                                 emacs_value args[],
                                 void UNUSED(*data_ptr)) {
  emacs_value COLLECTION = args[0];
  emacs_value QUERY = args[1];

  // Early exit if QUERY is empty.
  ptrdiff_t query_len;
  env->copy_string_contents(env, QUERY, NULL, &query_len);
  if (query_len == /* solely null byte */ 1) {
    emacs_value log1 = env->make_string (env, "QUERY is nil.", 13);
    env->funcall(env, Fmessage, 1, &log1);
    return Qnil;
  }

  // Early exit if COLLECTION is NULL.
  if (!env->is_not_nil(env, COLLECTION)) {
    emacs_value log1 = env->make_string (env, "COLLECTION is nil.", 18);
    env->funcall(env, Fmessage, 1, &log1);
    return Qnil;
  }

  fzf_slab_t *slab;
  if (nargs > 2) {
    // Re-use SLAB argument.
    slab = env->get_user_ptr(env, args[2]);
  } else {
    // Create a one-time use slab.
    slab = fzf_make_default_slab();
  }

  int success = false;
  emacs_value *results_array;
  struct Bump *bump = NULL;
  ptrdiff_t results_offset = 0;

  emacs_value length_result = env->funcall(env, Flength, 1, &COLLECTION);
  ptrdiff_t collection_size = env->extract_integer(env, length_result);
  results_array = malloc(sizeof(emacs_value) * collection_size);

  ptrdiff_t idx = 0;
  emacs_value candidate_str;

  while (idx < collection_size) {
    emacs_value collection_idx =  env->make_integer(env, idx++);
    candidate_str = env->funcall(env, Fnth, 2, (emacs_value[]) {
        collection_idx,
        COLLECTION,
      });

    struct Str str = copy_emacs_string(env, &bump, candidate_str);
    if (!str.b) { goto err; }

    struct Str query = copy_emacs_string(env, &bump, QUERY);
    if (!query.b) { goto err; }

    /* fzf_case_mode enum : CaseSmart = 0, CaseIgnore, CaseRespect
     * normalize bool     : Always set to false because its not implemented yet.
     *                      This is reserved for future use
     * pattern char*      : Pattern you want to match. e.g. "src | lua !.c$
     * fuzzy bool         : Enable or disable fuzzy matching
     */
    fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, false, query.b, true);

    /* You can get the score/position for as many items as you want */
    int score = fzf_get_score(str.b, pattern, slab);
    fzf_position_t *pos = fzf_get_positions(str.b, pattern, slab);

    size_t offset = 2; // The candidate string and its score.
    size_t len = 0;
    if (pos) {
      len = pos->size;
    }

    emacs_value *result_array = malloc(sizeof(emacs_value) * (offset + len));

    result_array[0] = candidate_str;
    result_array[1] = env->make_integer(env, score);

    for (size_t i = 0; i < len; i++) {
      result_array[offset + i] = env->make_integer(env, pos->data[len - (i + 1)]);
    }

    emacs_value result = env->funcall(env, Flist, offset + len, result_array);

    results_array[results_offset++] = result;

    fzf_free_positions(pos);
    success = true;
  }

err:
  bump_free(bump);
  if (!success) {
    env->non_local_exit_signal(env, env->intern(env, "error"), Qnil);
  }

  emacs_value final_result = env->funcall(env,
                                          Flist,
                                          results_offset,
                                          results_array);
  return final_result;
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
  Qcons = env->make_global_ref(env, env->intern(env, "cons"));
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

  Qlistofzero = env->make_global_ref(
    env, env->funcall(env, Qcons, 2,
                      (emacs_value[]){env->make_integer(env, 0), Qnil}));

  return 0;
}
