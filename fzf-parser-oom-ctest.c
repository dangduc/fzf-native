// SPDX-License-Identifier: MIT
/* Exhaustively fail each allocation made by fzf_parse_pattern.  The parser
   must return NULL, release every earlier allocation, and never dereference
   the failed result.  ASan/LSan supplies the leak and invalid-access oracle. */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static size_t allocation_number;
static size_t fail_at;
static bool failure_injected;

static void *parser_test_malloc(size_t size) {
  size_t current = allocation_number++;
  if (current == fail_at) {
    failure_injected = true;
    return NULL;
  }
  return malloc(size);
}

static void *parser_test_calloc(size_t count, size_t size) {
  size_t current = allocation_number++;
  if (current == fail_at) {
    failure_injected = true;
    return NULL;
  }
  return calloc(count, size);
}

static void *parser_test_realloc(void *pointer, size_t size) {
  size_t current = allocation_number++;
  if (current == fail_at) {
    failure_injected = true;
    return NULL;
  }
  return realloc(pointer, size);
}

#define malloc(size) parser_test_malloc(size)
#define calloc(count, size) parser_test_calloc((count), (size))
#define realloc(pointer, size) parser_test_realloc((pointer), (size))
#include "fzf.c"
#undef malloc
#undef calloc
#undef realloc

static int check(bool condition, const char *message, size_t index) {
  if (condition) return 0;
  fprintf(stderr, "FAIL allocation %zu: %s\n", index, message);
  return 1;
}

int main(void) {
  size_t tested = 0;
  for (fail_at = 0;; fail_at++) {
    allocation_number = 0;
    failure_injected = false;
    char query[] = "alpha beta gamma delta";
    fzf_pattern_t *pattern = fzf_parse_pattern(
        CaseIgnore, false, query, true);

    if (!failure_injected) {
      int failed = check(pattern != NULL,
                         "successful allocation run returned NULL", fail_at);
      fzf_free_pattern(pattern);
      if (failed) return failed;
      break;
    }

    if (check(pattern == NULL,
              "injected allocation failure returned a pattern", fail_at)) {
      fzf_free_pattern(pattern);
      return 1;
    }
    tested++;
  }

  printf("parser OOM test passed (%zu allocation sites)\n", tested);
  return 0;
}
