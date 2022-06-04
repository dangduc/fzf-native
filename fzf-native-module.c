#include <stdlib.h>
#include <stdbool.h>
#include <stdalign.h>
#include <string.h>
#include <ctype.h>
#include <emacs-module.h>
#include "fzf.h"

int plugin_is_GPL_compatible;

#define MAX(a, b) ({ __typeof__(a) _a = (a), _b = (b); _a > _b ? _a : _b; })

/** An Emacs string made accessible by copying. */
struct EmacsStr {
  emacs_value value; ///< The original string value.
  size_t len; ///< The length of the string minus the null byte.
  char b[]; ///< The null-terminated copied string.
};

/** Module userdata that gets allocated once at initialization. */
struct Data {
};

/** Intrusive linked list of bump allocation blocks. */
struct Bump {
  struct Bump *next;
  size_t index, capacity;
  char b[];
};

/**
 * Allocates the specified number of bytes.
 *
 * Returns NULL on failure.
 */
static void *bump_alloc(struct Bump **head, size_t len) {
  if (!*head || (*head)->capacity - (*head)->index < len) {
    size_t capacity = MAX(*head ? 2 * (*head)->capacity : 1024, len);
    struct Bump *new_head;
    if (!(new_head = malloc(sizeof *new_head + capacity)))
      return NULL;
    *new_head = (struct Bump) { .next = *head, .index = 0, .capacity = capacity };
    *head = new_head;
  }

  void *p = (*head)->b + (*head)->index;
  (*head)->index += len;
  return p;
}

static void bump_free(struct Bump *head) {
  while (head) {
    struct Bump *next = head->next;
    free(head);
    head = next;
  }
}

/**
 * Copies the Emacs string to make its lifetime that of the allocator.
 */
static struct EmacsStr *copy_emacs_string(emacs_env *env, struct Bump **bump, emacs_value value) {
  ptrdiff_t len;
  // Determine the size of the string (including null-terminator)
  env->copy_string_contents(env, value, NULL, &len);

  struct EmacsStr *result;
  // Note: Since only EmacsStr:s are allocated with bump_alloc we
  // may use its smaller alignment rather than the scalar maximum.
  if (!(result = bump_alloc(bump, sizeof *result + len
                            + alignof(struct EmacsStr) - 1 & ~(alignof(struct EmacsStr) - 1))))
    return NULL;

  result->value = value;
  result->len = len - 1;
  env->copy_string_contents(env, value, result->b, &len);
  return result;
}

emacs_value fzf_native_score(emacs_env *env, ptrdiff_t nargs __attribute__ ((__unused__)), emacs_value args[], void *data_ptr) {
  emacs_value nil = env->intern(env, "nil");
  emacs_value result = nil;

  // Short-circuit if STR is empty
  ptrdiff_t str_len;
  env->copy_string_contents(env, args[0], NULL, &str_len);
  if (str_len == /* solely null byte */ 1) {
    result = env->make_integer(env, 0);
    return result;
  }

  // Short-circuit if QUERY is empty
  ptrdiff_t query_len;
  env->copy_string_contents(env, args[1], NULL, &query_len);
  if (query_len == /* solely null byte */ 1) {
    result = env->make_integer(env, 0);
    return result;
  }

  emacs_value fcons = env->intern(env, "cons");

  struct Bump *bump = NULL;

  struct EmacsStr *str = copy_emacs_string(env, &bump, args[0]);
  if (!str) goto error;

  struct EmacsStr *query = copy_emacs_string(env, &bump, args[1]);
  if (!query) goto error;

  fzf_slab_t *slab = fzf_make_default_slab();
  /* fzf_case_mode enum : CaseSmart = 0, CaseIgnore, CaseRespect
   * normalize bool     : always set to false because its not implemented yet.
   *                      This is reserved for future use
   * pattern char*      : pattern you want to match. e.g. "src | lua !.c$
   * fuzzy bool         : enable or disable fuzzy matching
   */
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseSmart, false, query->b, true);

  /* you can get the score/position for as many items as you want */
  int score = fzf_get_score(str->b, pattern, slab);
  fzf_position_t *pos = fzf_get_positions(str->b, pattern, slab);

  if (pos) {
    for (size_t i = pos->size; i-- > 0;) {
      emacs_value p = env->make_integer(env, pos->data[i]);
      result = env->funcall(env, fcons, 2, (emacs_value[]){p, result});
    }
  }

  emacs_value s = env->make_integer(env, score);
  result = env->funcall(env, fcons, 2, (emacs_value[]) {s, result});

  fzf_free_positions(pos);
  fzf_free_pattern(pattern);
  fzf_free_slab(slab);

 error:
  return result;
}
int emacs_module_init(struct emacs_runtime *rt) {
  // Verify compatability with Emacs executable loading this module
  if ((size_t) rt->size < sizeof *rt)
    return 1;
  emacs_env *env = rt->get_environment(rt);
  if ((size_t) env->size < sizeof *env)
    return 2;

  static struct Data data;

  env->funcall(env, env->intern(env, "defalias"), 2, (emacs_value[]) {
      env->intern(env, "fzf-native-score"),
      env->make_function(env, 2, 2, fzf_native_score,
                         "Score STR matching QUERY.\n"
                         "\n"
                         "\(fn STR QUERY)",
                         &data),
    });

  env->funcall(env, env->intern(env, "provide"), 1,
               (emacs_value[]) { env->intern(env, "fzf-native-module") });

  return 0;
}
