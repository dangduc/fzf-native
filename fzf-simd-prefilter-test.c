// SPDX-License-Identifier: GPL-3.0-or-later
/* Direct parity tests for the bounded ASCII SIMD prefilters. */

#include "fzf-additions.h"
#include "fzf-simd-prefilter.h"

#include <inttypes.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures;

#define CHECK(condition, message)                                              \
  do {                                                                         \
    if (!(condition)) {                                                        \
      fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__, (message));     \
      failures++;                                                              \
    }                                                                          \
  } while (0)

static uint64_t random_state = UINT64_C(0x5df0a74e9b1632c1);

static uint32_t random_u32(void) {
  random_state ^= random_state >> 12;
  random_state ^= random_state << 25;
  random_state ^= random_state >> 27;
  return (uint32_t)((random_state * UINT64_C(2685821657736338717)) >> 32);
}

#if FZF_HAVE_SIMD_PREFILTER

static bool scalar_byte_matches(uint8_t candidate, uint8_t pattern,
                                bool case_sensitive) {
  return candidate == pattern ||
         (!case_sensitive && pattern >= 'a' && pattern <= 'z' &&
          candidate == (uint8_t)(pattern - ('a' - 'A')));
}

static ptrdiff_t scalar_fuzzy_index(const uint8_t *text, size_t text_size,
                                    const uint8_t *pattern,
                                    size_t pattern_size,
                                    bool case_sensitive) {
  if (pattern_size == 0) return 0;
  size_t pattern_index = 0;
  size_t first = 0;
  for (size_t i = 0; i < text_size; i++) {
    if (!scalar_byte_matches(text[i], pattern[pattern_index],
                             case_sensitive))
      continue;
    if (pattern_index == 0) first = i;
    pattern_index++;
    if (pattern_index == pattern_size)
      return first == 0 ? 0 : (ptrdiff_t)(first - 1);
  }
  return -1;
}

static ptrdiff_t planned_fuzzy_index(const uint8_t *text, size_t text_size,
                                     const fzf_ascii_query_plan_t *plan) {
  if (plan->pattern_size == 0) return 0;
  const char *first = fzf_ascii_plan_find_initial_byte(
      (const char *)text, text_size, &plan->bytes[0], plan->case_sensitive);
  if (!first) return -1;
  size_t first_index = (size_t)(first - (const char *)text);
  if (!fzf_ascii_plan_ordered_after_first(
          (const char *)text, text_size, plan, first_index))
    return -1;
  return first_index == 0 ? 0 : (ptrdiff_t)(first_index - 1);
}

static const char *scalar_find_byte_two(const char *text, size_t text_size,
                                        uint8_t exact_byte,
                                        uint8_t alternate_byte) {
  for (size_t i = 0; i < text_size; i++) {
    uint8_t byte = (uint8_t)text[i];
    if (byte == exact_byte || byte == alternate_byte) return text + i;
  }
  return NULL;
}

static void check_byte_two(const char *text, size_t text_size,
                           uint8_t exact_byte, uint8_t alternate_byte) {
  const char *scalar = scalar_find_byte_two(
      text, text_size, exact_byte, alternate_byte);
  const char *simd = fzf_simd_find_byte_two(
      text, text_size, exact_byte, alternate_byte);
  if (scalar != simd) {
    fprintf(stderr,
            "FAIL paired byte search: size=%zu exact=%u alternate=%u "
            "scalar=%td simd=%td\n",
            text_size, (unsigned int)exact_byte, (unsigned int)alternate_byte,
            scalar ? scalar - text : -1, simd ? simd - text : -1);
    failures++;
  }
  if (text_size > FZF_SIMD_LANES &&
      text_size < 2 * FZF_SIMD_LANES) {
    const char *short_simd = fzf_simd_find_byte_two_short(
        text, text_size, exact_byte, alternate_byte);
    if (scalar != short_simd) {
      fprintf(stderr,
              "FAIL short paired byte search: size=%zu exact=%u alternate=%u "
              "scalar=%td simd=%td\n",
              text_size, (unsigned int)exact_byte,
              (unsigned int)alternate_byte,
              scalar ? scalar - text : -1,
              short_simd ? short_simd - text : -1);
      failures++;
    }
  }
}

static void test_paired_byte_search(void) {
  for (size_t text_size = 0; text_size <= 256; text_size++) {
    char *text = malloc(text_size == 0 ? 1 : text_size);
    CHECK(text != NULL, "paired byte search allocation failed");
    if (!text) return;
    memset(text, 'x', text_size);
    check_byte_two(text, text_size, 'q', 'Q');
    check_byte_two(text, text_size, 'q', 'q');
    for (size_t position = 0; position < text_size; position++) {
      text[position] = 'q';
      check_byte_two(text, text_size, 'q', 'Q');
      text[position] = 'Q';
      check_byte_two(text, text_size, 'q', 'Q');
      text[position] = 'x';
    }
    if (text_size > 1) {
      text[text_size - 1] = 'q';
      for (size_t position = 0; position + 1 < text_size; position++) {
        text[position] = 'Q';
        check_byte_two(text, text_size, 'q', 'Q');
        text[position] = 'x';
      }
    }
    free(text);
  }
}

static bool scalar_matches_at(const uint8_t *text, const uint8_t *pattern,
                              size_t pattern_size, bool case_sensitive) {
  for (size_t i = 0; i < pattern_size; i++)
    if (!scalar_byte_matches(text[i], pattern[i], case_sensitive)) return false;
  return true;
}

static size_t scalar_find_exact(const uint8_t *text, size_t text_size,
                                const uint8_t *pattern, size_t pattern_size,
                                bool case_sensitive, size_t from) {
  if (pattern_size == 0) return from <= text_size ? from : SIZE_MAX;
  if (text_size < pattern_size || from > text_size - pattern_size)
    return SIZE_MAX;
  size_t last = text_size - pattern_size;
  for (size_t i = from; i <= last; i++)
    if (scalar_matches_at(text + i, pattern, pattern_size, case_sensitive))
      return i;
  return SIZE_MAX;
}

static fzf_ascii_query_plan_t *make_plan(const uint8_t *pattern,
                                         size_t pattern_size,
                                         bool case_sensitive) {
  size_t size = fzf_ascii_query_plan_size(pattern_size);
  if (size == 0) return NULL;
  fzf_ascii_query_plan_t *plan = malloc(size);
  if (!plan) return NULL;
  fzf_ascii_query_plan_init(plan, (const char *)pattern, pattern_size,
                            case_sensitive);
  return plan;
}

static void compare_one_case(size_t text_size, size_t pattern_size,
                             bool case_sensitive) {
  static const uint8_t lower_alphabet[] = "abcdeghikmnoprstuvxyz012/_-.";
  static const uint8_t mixed_alphabet[] =
      "aaabbbcccdddeeeABCXYZ012/_-.[]{}";
  uint8_t *text = malloc(text_size == 0 ? 1 : text_size);
  uint8_t *pattern = malloc(pattern_size == 0 ? 1 : pattern_size);
  CHECK(text != NULL && pattern != NULL, "test allocation failed");
  if (!text || !pattern) {
    free(pattern);
    free(text);
    return;
  }

  for (size_t i = 0; i < text_size; i++)
    text[i] = mixed_alphabet[random_u32() % (sizeof mixed_alphabet - 1)];
  const uint8_t *alphabet = case_sensitive ? mixed_alphabet : lower_alphabet;
  size_t alphabet_size = case_sensitive ? sizeof mixed_alphabet - 1
                                        : sizeof lower_alphabet - 1;
  for (size_t i = 0; i < pattern_size; i++)
    pattern[i] = alphabet[random_u32() % alphabet_size];

  /* Force both contiguous and gapped matches into a useful share of cases. */
  if (pattern_size <= text_size && (random_u32() & 1) != 0) {
    size_t start = random_u32() % (text_size - pattern_size + 1);
    for (size_t i = 0; i < pattern_size; i++) {
      uint8_t byte = pattern[i];
      if (!case_sensitive && byte >= 'a' && byte <= 'z' &&
          (random_u32() & 1) != 0)
        byte = (uint8_t)(byte - ('a' - 'A'));
      text[start + i] = byte;
    }
  }

  fzf_ascii_query_plan_t *plan =
      make_plan(pattern, pattern_size, case_sensitive);
  CHECK(plan != NULL, "plan allocation failed");
  if (!plan) {
    free(pattern);
    free(text);
    return;
  }

  ptrdiff_t scalar_fuzzy = scalar_fuzzy_index(
      text, text_size, pattern, pattern_size, case_sensitive);
  ptrdiff_t simd_fuzzy = planned_fuzzy_index(text, text_size, plan);
  if (scalar_fuzzy != simd_fuzzy) {
    fprintf(stderr,
            "FAIL fuzzy parity: tn=%zu pn=%zu case=%d scalar=%td simd=%td\n",
            text_size, pattern_size, (int)case_sensitive, scalar_fuzzy,
            simd_fuzzy);
    failures++;
  }

  for (size_t from = 0; from <= text_size + 1; from++) {
    size_t scalar = scalar_find_exact(text, text_size, pattern, pattern_size,
                                      case_sensitive, from);
    size_t simd = fzf_ascii_plan_find_exact(
        (const char *)text, text_size, plan, from);
    if (scalar != simd) {
      fprintf(stderr,
              "FAIL exact parity: tn=%zu pn=%zu from=%zu case=%d "
              "scalar=%zu simd=%zu\n",
              text_size, pattern_size, from, (int)case_sensitive, scalar,
              simd);
      failures++;
      break;
    }
  }

  if (pattern_size >= 2) {
    CHECK(plan->seed_a_offset < plan->seed_b_offset,
          "seed offsets are not ordered and distinct");
    CHECK(plan->seed_b_offset < pattern_size,
          "seed offset exceeds pattern");
  }
  free(plan);
  free(pattern);
  free(text);
}

static void test_seed_selection(void) {
  size_t a = SIZE_MAX;
  size_t b = SIZE_MAX;
  fzf_ascii_seed_offsets((const uint8_t *)"aaaaazaa", 8, &a, &b);
  CHECK(a == 0 && b == 5, "rare z seed was not selected");
  fzf_ascii_seed_offsets((const uint8_t *)"aaaa", 4, &a, &b);
  CHECK(a == 0 && b == 1, "duplicate-byte seed offsets changed");
}

static void check_occurrences(const char *text, const char *pattern,
                              bool case_sensitive, const size_t *expected,
                              size_t expected_size) {
  size_t text_size = strlen(text);
  size_t pattern_size = strlen(pattern);
  fzf_ascii_query_plan_t *plan = make_plan(
      (const uint8_t *)pattern, pattern_size, case_sensitive);
  CHECK(plan != NULL, "occurrence plan allocation failed");
  if (!plan) return;

  size_t actual[64];
  size_t actual_size = 0;
  size_t from = 0;
  while (from <= text_size - pattern_size) {
    size_t position = fzf_ascii_plan_find_exact(
        text, text_size, plan, from);
    if (position == SIZE_MAX) break;
    CHECK(actual_size < sizeof actual / sizeof actual[0],
          "too many exact occurrences");
    if (actual_size >= sizeof actual / sizeof actual[0]) break;
    actual[actual_size++] = position;
    from = position + 1;
  }
  CHECK(actual_size == expected_size, "exact occurrence count differs");
  if (actual_size == expected_size)
    for (size_t i = 0; i < expected_size; i++)
      CHECK(actual[i] == expected[i], "exact occurrence position differs");
  free(plan);
}

static void test_complete_occurrence_sets(void) {
  static const size_t overlap[] = {0, 1, 2, 3};
  static const size_t repeated[] = {0, 3, 6};
  check_occurrences("aaaaa", "aa", true, overlap,
                    sizeof overlap / sizeof overlap[0]);
  check_occurrences("aAaAa", "aa", false, overlap,
                    sizeof overlap / sizeof overlap[0]);
  check_occurrences("abcabcabc", "abc", true, repeated,
                    sizeof repeated / sizeof repeated[0]);
}

static void test_first_index_semantics(void) {
  struct Case {
    const char *text;
    const char *pattern;
    bool case_sensitive;
    ptrdiff_t expected;
  } cases[] = {
      {"abc", "abc", true, 0},
      {"xxa---b---c", "abc", true, 1},
      {"xxA---B---c", "abc", false, 1},
      {"a----------------b----------------c", "abc", true, 0},
      {"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", "abc", true, -1},
  };
  for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
    const struct Case *c = &cases[i];
    fzf_ascii_query_plan_t *plan = make_plan(
        (const uint8_t *)c->pattern, strlen(c->pattern), c->case_sensitive);
    CHECK(plan != NULL, "first-index plan allocation failed");
    if (!plan) continue;
    ptrdiff_t actual = planned_fuzzy_index(
        (const uint8_t *)c->text, strlen(c->text), plan);
    CHECK(actual == c->expected, "first-index result changed");
    free(plan);
  }
}

static void test_boundary_lengths_and_safe_tails(void) {
  static const size_t lengths[] = {
      0,  1,  2,  7,  15, 16, 17, 23, 24,
      31, 32, 33, 47, 48, 49, 63, 64, 65, 95, 96, 97,
  };
  for (size_t li = 0; li < sizeof lengths / sizeof lengths[0]; li++) {
    size_t text_size = lengths[li];
    for (size_t round = 0; round < 32; round++) {
      size_t pattern_size = random_u32() % 33;
      compare_one_case(text_size, pattern_size, false);
      compare_one_case(text_size, pattern_size, true);
    }
  }
  for (size_t i = 0; i < 2500; i++)
    compare_one_case(random_u32() % 193, random_u32() % 49,
                     (random_u32() & 1) != 0);
}

#endif /* FZF_HAVE_SIMD_PREFILTER */

static fzf_term_t *only_term(fzf_pattern_t *pattern) {
  if (!pattern || pattern->size != 1 || !pattern->ptr[0] ||
      pattern->ptr[0]->size != 1)
    return NULL;
  return &pattern->ptr[0]->ptr[0];
}

static void test_parser_case_modes(void) {
  struct Case {
    const char *query;
    fzf_case_types mode;
    bool expected_case_sensitive;
  } cases[] = {
      {"abc", CaseSmart, false},
      {"Abc", CaseSmart, true},
      {"ABC", CaseIgnore, false},
      {"abc", CaseRespect, true},
  };
  for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
    char query[8];
    strcpy(query, cases[i].query);
    fzf_pattern_t *parsed = fzf_parse_pattern(
        cases[i].mode, false, query, true);
    fzf_term_t *term = only_term(parsed);
    CHECK(term != NULL, "case-mode query did not parse as one term");
    if (term) {
      CHECK(term->case_sensitive == cases[i].expected_case_sensitive,
            "parser case mode changed");
#if FZF_HAVE_SIMD_PREFILTER
      const fzf_ascii_query_plan_t *plan =
          fzf_ascii_query_plan_from_parsed(term->text);
      CHECK(plan->pattern_size == strlen(cases[i].query),
            "case-mode plan was unexpectedly disabled");
      CHECK(plan->case_sensitive == term->case_sensitive,
            "plan and term case modes differ");
#endif
    }
    fzf_free_pattern(parsed);
  }
}

static void test_long_query_scalar_fallback(void) {
  const size_t enabled_size = FZF_SIMD_MAX_PATTERN;
  const size_t disabled_size = FZF_SIMD_MAX_PATTERN + 1;
  char *enabled = malloc(enabled_size + 1);
  char *disabled = malloc(disabled_size + 1);
  CHECK(enabled != NULL && disabled != NULL, "long-query allocation failed");
  if (!enabled || !disabled) {
    free(disabled);
    free(enabled);
    return;
  }
  memset(enabled, 'a', enabled_size);
  memset(disabled, 'a', disabled_size);
  enabled[enabled_size] = 0;
  disabled[disabled_size] = 0;

  fzf_pattern_t *enabled_pattern = fzf_parse_pattern(
      CaseRespect, false, enabled, true);
  fzf_pattern_t *disabled_pattern = fzf_parse_pattern(
      CaseRespect, false, disabled, true);
  fzf_term_t *enabled_term = only_term(enabled_pattern);
  fzf_term_t *disabled_term = only_term(disabled_pattern);
  CHECK(enabled_term != NULL && disabled_term != NULL,
        "long query did not parse");
#if FZF_HAVE_SIMD_PREFILTER
  if (enabled_term) {
    const fzf_ascii_query_plan_t *plan =
        fzf_ascii_query_plan_from_parsed(enabled_term->text);
    CHECK(plan->pattern_size == enabled_size,
          "maximum-size plan was disabled");
  }
  if (disabled_term) {
    const fzf_ascii_query_plan_t *plan =
        fzf_ascii_query_plan_from_parsed(disabled_term->text);
    CHECK(plan->pattern_size == 0, "oversize plan was not disabled");
  }
#endif
  if (disabled_pattern) {
    CHECK(fzf_has_match(disabled, disabled_pattern, NULL),
          "disabled long-query scalar membership fallback failed");
    CHECK(fzf_get_score(disabled, disabled_pattern, NULL) > 0,
          "disabled long-query scalar scoring fallback failed");
  }
  fzf_free_pattern(disabled_pattern);
  fzf_free_pattern(enabled_pattern);
  free(disabled);
  free(enabled);
}

int main(void) {
#if FZF_HAVE_SIMD_PREFILTER
  test_paired_byte_search();
  test_seed_selection();
  test_complete_occurrence_sets();
  test_first_index_semantics();
  test_boundary_lengths_and_safe_tails();
#else
  puts("SIMD prefilter unavailable; direct helper checks skipped");
#endif
  test_parser_case_modes();
  test_long_query_scalar_fallback();
  if (failures != 0) {
    fprintf(stderr, "%d SIMD prefilter test(s) failed\n", failures);
    return 1;
  }
  puts("All SIMD prefilter tests passed.");
  return 0;
}
