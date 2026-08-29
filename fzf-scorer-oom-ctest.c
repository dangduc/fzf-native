// SPDX-License-Identifier: MIT
/* Exhaustively fail allocations reached by score and position evaluation.
   ASan/LSan is the memory-safety/leak oracle; the public thread-local error
   flag is the semantic oracle that distinguishes OOM from an ordinary miss. */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static size_t allocation_number;
static size_t fail_at;
static bool failure_injected;

static void *scorer_test_malloc(size_t size) {
  size_t current = allocation_number++;
  if (current == fail_at) {
    failure_injected = true;
    return NULL;
  }
  return malloc(size);
}

static void *scorer_test_calloc(size_t count, size_t size) {
  size_t current = allocation_number++;
  if (current == fail_at) {
    failure_injected = true;
    return NULL;
  }
  return calloc(count, size);
}

static void *scorer_test_realloc(void *pointer, size_t size) {
  size_t current = allocation_number++;
  if (current == fail_at) {
    failure_injected = true;
    return NULL;
  }
  return realloc(pointer, size);
}

#define malloc(size) scorer_test_malloc(size)
#define calloc(count, size) scorer_test_calloc((count), (size))
#define realloc(pointer, size) scorer_test_realloc((pointer), (size))
#include "fzf.c"
#undef malloc
#undef calloc
#undef realloc

static int fail(const char *kind, size_t index, const char *message) {
  fprintf(stderr, "FAIL %s allocation %zu: %s\n", kind, index, message);
  return 1;
}

static fzf_pattern_t *make_pattern(char *query) {
  fail_at = SIZE_MAX;
  allocation_number = 0;
  failure_injected = false;
  return fzf_parse_pattern(CaseRespect, false, query, true);
}

static int exercise_score(const char *label, const char *candidate,
                          fzf_pattern_t *pattern, size_t *tested) {
  for (fail_at = 0;; fail_at++) {
    allocation_number = 0;
    failure_injected = false;
    int32_t score = fzf_get_score(candidate, pattern, NULL);
    if (!failure_injected) {
      if (score <= 0) return fail(label, fail_at, "successful run did not match");
      if (fzf_allocation_failed())
        return fail(label, fail_at, "successful run retained OOM flag");
      return 0;
    }
    if (score != 0)
      return fail(label, fail_at, "injected OOM returned a match score");
    if (!fzf_allocation_failed())
      return fail(label, fail_at, "injected OOM was reported as no-match");
    (*tested)++;
  }
}

static int exercise_positions(const char *label, const char *candidate,
                              fzf_pattern_t *pattern, size_t *tested) {
  for (fail_at = 0;; fail_at++) {
    allocation_number = 0;
    failure_injected = false;
    fzf_position_t *positions =
        fzf_get_positions(candidate, pattern, NULL);
    if (!failure_injected) {
      if (!positions || positions->size == 0) {
        fzf_free_positions(positions);
        return fail(label, fail_at, "successful run returned no positions");
      }
      if (fzf_allocation_failed()) {
        fzf_free_positions(positions);
        return fail(label, fail_at, "successful run retained OOM flag");
      }
      fzf_free_positions(positions);
      return 0;
    }
    if (positions != NULL) {
      fzf_free_positions(positions);
      return fail(label, fail_at, "injected OOM returned partial positions");
    }
    if (!fzf_allocation_failed())
      return fail(label, fail_at, "injected OOM was reported as no positions");
    (*tested)++;
  }
}

int main(void) {
  size_t tested = 0;
  char ascii_query[] = "abc";
  char utf8_query[] = "你界";
  fzf_pattern_t *ascii_pattern = make_pattern(ascii_query);
  fzf_pattern_t *utf8_pattern = make_pattern(utf8_query);
  if (!ascii_pattern || !utf8_pattern) {
    fzf_free_pattern(ascii_pattern);
    fzf_free_pattern(utf8_pattern);
    return fail("setup", 0, "could not create patterns");
  }

  int result =
      exercise_score("ASCII score", "alphabet-bravo-charlie", ascii_pattern,
                     &tested) ||
      exercise_positions("ASCII positions", "alphabet-bravo-charlie",
                         ascii_pattern, &tested) ||
      exercise_score("UTF-8 score", "a你---界z", utf8_pattern, &tested) ||
      exercise_positions("UTF-8 positions", "a你---界z", utf8_pattern,
                         &tested);

  fail_at = SIZE_MAX;
  fzf_free_pattern(ascii_pattern);
  fzf_free_pattern(utf8_pattern);
  if (result) return result;
  printf("scorer OOM test passed (%zu allocation failures)\n", tested);
  return 0;
}
