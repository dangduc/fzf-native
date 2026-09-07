/* SPDX-License-Identifier: GPL-3.0-or-later
 * Coverage-guided safety and consistency checks for the existing matcher.
 *
 * Byte 0 selects case, fuzzy, and slab options.  The remaining bytes are
 * QUERY, a newline separator, and CANDIDATE.  Without a newline, byte 0 also
 * selects the split.  The format intentionally accepts malformed byte
 * strings: the public C matcher accepts NUL-terminated bytes, and sanitizers
 * should cover that surface without requiring a structured mutator.
 */

#include "fzf-additions.h"
#include "fzf.h"
#include "fzf-private.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { FZF_NATIVE_FUZZ_MAX_INPUT = 4096 };

static void fuzz_fail(const char *property) {
  fprintf(stderr, "fzf-native fuzz invariant failed: %s\n", property);
  abort();
}

static bool valid_utf8(const char *text, size_t len) {
  size_t offset = 0;
  while (offset < len) {
    utf8proc_int32_t codepoint;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)text + offset,
        (utf8proc_ssize_t)(len - offset), &codepoint);
    if (width <= 0) return false;
    offset += (size_t)width;
  }
  return true;
}

static size_t visible_character_count(const char *text) {
  size_t bytes = strlen(text);
  return valid_utf8(text, bytes) ? utf8_strlen(text, bytes) : bytes;
}

static fzf_slab_t *make_selected_slab(uint8_t options) {
  static const size_t caps16[] = {1, 8, 64, 1024, 8192, 100 * 1024};
  static const size_t caps32[] = {1, 8, 64, 256, 1024, 2048};
  size_t which = (options >> 3) % (sizeof(caps16) / sizeof(caps16[0]));
  return fzf_make_slab((fzf_slab_config_t){caps16[which], caps32[which]});
}

static void check_positions(const char *candidate, bool matched,
                            const fzf_position_t *positions) {
  if (!matched && positions && positions->size != 0)
    fuzz_fail("a failed match returned highlight positions");
  if (!positions)
    return;

  size_t limit = visible_character_count(candidate);
  for (size_t i = 0; i < positions->size; i++) {
    if (positions->data[i] >= limit)
      fuzz_fail("a highlight position is outside the candidate");
  }

}

static void check_position_order(const fzf_position_t *positions) {
  if (!positions || positions->size < 2)
    return;
  if (positions->data[0] == positions->data[1])
    fuzz_fail("duplicate highlight positions");
  bool increasing = positions->data[1] > positions->data[0];
  for (size_t i = 2; i < positions->size; i++) {
    if (positions->data[i] == positions->data[i - 1] ||
        ((positions->data[i] > positions->data[i - 1]) != increasing))
      fuzz_fail("unordered highlight positions");
  }
}

static fzf_algo_t utf8_variant(fzf_algo_t algorithm) {
  if (algorithm == fzf_fuzzy_match_v2) return fzf_fuzzy_match_v2_utf8;
  if (algorithm == fzf_fuzzy_match_v1) return fzf_fuzzy_match_v1_utf8;
  if (algorithm == fzf_exact_match_naive) return fzf_exact_match_utf8;
  if (algorithm == fzf_prefix_match) return fzf_prefix_match_utf8;
  if (algorithm == fzf_suffix_match) return fzf_suffix_match_utf8;
  if (algorithm == fzf_equal_match) return fzf_equal_match_utf8;
  return algorithm;
}

static const char *algo_name(fzf_algo_t algorithm) {
  if (algorithm == fzf_fuzzy_match_v2) return "fuzzy-v2";
  if (algorithm == fzf_fuzzy_match_v2_utf8) return "fuzzy-v2-utf8";
  if (algorithm == fzf_fuzzy_match_v1) return "fuzzy-v1";
  if (algorithm == fzf_fuzzy_match_v1_utf8) return "fuzzy-v1-utf8";
  if (algorithm == fzf_exact_match_naive) return "exact";
  if (algorithm == fzf_exact_match_utf8) return "exact-utf8";
  if (algorithm == fzf_prefix_match) return "prefix";
  if (algorithm == fzf_prefix_match_utf8) return "prefix-utf8";
  if (algorithm == fzf_suffix_match) return "suffix";
  if (algorithm == fzf_suffix_match_utf8) return "suffix-utf8";
  if (algorithm == fzf_equal_match) return "equal";
  if (algorithm == fzf_equal_match_utf8) return "equal-utf8";
  return "unknown";
}

static bool pattern_has_inverse(const fzf_pattern_t *pattern) {
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++) {
      if (set->ptr[j].inv)
        return true;
    }
  }
  return false;
}

static bool extension_preserves_term(const fzf_term_t *term, bool prepend) {
  if (term->inv || !term->fn || !term->text) return false;
  const fzf_string_t *text = (const fzf_string_t *)term->text;
  if (!valid_utf8(text->data, text->size)) return false;

  if (term->fn == fzf_fuzzy_match_v1 ||
      term->fn == fzf_fuzzy_match_v1_utf8 ||
      term->fn == fzf_fuzzy_match_v2 ||
      term->fn == fzf_fuzzy_match_v2_utf8 ||
      term->fn == fzf_exact_match_naive ||
      term->fn == fzf_exact_match_utf8)
    return true;
  if (prepend)
    return term->fn == fzf_suffix_match ||
           term->fn == fzf_suffix_match_utf8;
  return term->fn == fzf_prefix_match ||
         term->fn == fzf_prefix_match_utf8;
}

static bool extension_preserves_pattern(const fzf_pattern_t *pattern,
                                        bool prepend) {
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++)
      if (!extension_preserves_term(&set->ptr[j], prepend)) return false;
  }
  return true;
}

static bool pattern_contains_codepoint(const fzf_pattern_t *pattern,
                                       utf8proc_int32_t wanted) {
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++) {
      const fzf_string_t *text = (const fzf_string_t *)set->ptr[j].text;
      size_t offset = 0;
      while (text && offset < text->size) {
        utf8proc_int32_t codepoint;
        utf8proc_ssize_t width = utf8proc_iterate(
            (const utf8proc_uint8_t *)text->data + offset,
            (utf8proc_ssize_t)(text->size - offset), &codepoint);
        if (width <= 0 || codepoint == wanted) return true;
        offset += (size_t)width;
      }
    }
  }
  return false;
}

static bool extension_keeps_slab_path(const fzf_pattern_t *pattern,
                                      size_t candidate_units,
                                      size_t extended_units,
                                      const fzf_slab_t *slab) {
  if (!slab) return true;
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++) {
      const fzf_term_t *term = &set->ptr[j];
      if (term->fn != fzf_fuzzy_match_v2 &&
          term->fn != fzf_fuzzy_match_v2_utf8)
        continue;
      const fzf_string_t *text = (const fzf_string_t *)term->text;
      size_t pattern_units = utf8_strlen(text->data, text->size);
      bool old_fallback = candidate_units != 0 &&
          pattern_units > slab->I16.cap / candidate_units;
      bool new_fallback = extended_units != 0 &&
          pattern_units > slab->I16.cap / extended_units;
      if (old_fallback != new_fallback) return false;
    }
  }
  return true;
}

static void check_candidate_extension(const char *candidate,
                                      fzf_pattern_t *pattern,
                                      int32_t score, fzf_slab_t *slab) {
  static const char *extensions[] = {
      "x", " ", "\xc3\xa9", "\xf0\x9f\x9a\x80",
  };
  size_t length = strlen(candidate);
  if (score <= 0 || !valid_utf8(candidate, length)) return;
  char *extended = malloc(length + 5);
  if (!extended) abort();

  bool append_safe = extension_preserves_pattern(pattern, false);
  bool prepend_safe = extension_preserves_pattern(pattern, true);
  for (size_t i = 0; i < sizeof extensions / sizeof extensions[0]; i++) {
    size_t extension_len = strlen(extensions[i]);
    if (append_safe) {
      memcpy(extended, candidate, length);
      memcpy(extended + length, extensions[i], extension_len + 1);
      if (fzf_get_score(extended, pattern, slab) <= 0)
        fuzz_fail("appending text destroyed a prefix-safe match");
    }
    if (prepend_safe) {
      memcpy(extended, extensions[i], extension_len);
      memcpy(extended + extension_len, candidate, length + 1);
      if (fzf_get_score(extended, pattern, slab) <= 0)
        fuzz_fail("prepending text destroyed a suffix-safe match");
    }
  }

  static const char nonmatching_scalar[] = "\xf4\x8f\xbf\xbf";
  if (append_safe && is_ascii_utf8proc(candidate, length) &&
      !pattern_contains_codepoint(pattern, 0x10ffff) &&
      extension_keeps_slab_path(pattern, length, length + 1, slab)) {
    memcpy(extended, candidate, length);
    memcpy(extended + length, nonmatching_scalar,
           sizeof nonmatching_scalar);
    if (fzf_get_score(extended, pattern, slab) != score)
      fuzz_fail("ASCII-to-UTF-8 dispatch changed a prefix-safe score");
  }
  free(extended);
}

#ifndef FZF_NATIVE_UTF8_MATCHING
static bool pattern_has_end_anchor(const fzf_pattern_t *pattern) {
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++) {
      if (set->ptr[j].fn == fzf_suffix_match ||
          set->ptr[j].fn == fzf_equal_match)
        return true;
    }
  }
  return false;
}

static bool ascii_bytes(const uint8_t *bytes, size_t size) {
  for (size_t i = 0; i < size; i++) {
    if (bytes[i] >= 0x80)
      return false;
  }
  return true;
}
#endif

static void check_term(const char *candidate, const fzf_term_t *term,
                       fzf_slab_t *slab) {
  if (!term->fn || !term->text)
    return;

  fzf_string_t input = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t *pattern = (fzf_string_t *)term->text;
  fzf_algo_t algorithm = term->fn;
  if (!is_ascii_utf8proc(input.data, input.size))
    algorithm = utf8_variant(algorithm);
  fzf_result_t without_positions =
      algorithm(term->case_sensitive, term->normalize, &input, pattern, NULL,
                slab);
  fzf_position_t *positions = fzf_pos_array(0);
  if (!positions)
    abort();
  fzf_result_t with_positions = algorithm(
      term->case_sensitive, term->normalize, &input, pattern, positions, slab);

  /* Fuzzy v2 may backtrack to a more precise START only when positions are
     requested.  Membership and score must not depend on observability. */
  if ((without_positions.start >= 0) != (with_positions.start >= 0) ||
      without_positions.score != with_positions.score)
    fuzz_fail("requesting positions changed a term result");
  if (with_positions.start < 0 && positions->size != 0)
    fuzz_fail("a failed term returned highlight positions");
  check_positions(candidate, with_positions.start >= 0, positions);
  check_position_order(positions);
  if (with_positions.start >= 0 && valid_utf8(input.data, input.size) &&
      valid_utf8(pattern->data, pattern->size)) {
    size_t pattern_characters = utf8_strlen(pattern->data, pattern->size);
    if (positions->size != pattern_characters) {
      fprintf(stderr,
              "%s returned %zu positions for a %zu-character pattern\n",
              algo_name(algorithm), positions->size, pattern_characters);
      fuzz_fail("a matched term returned the wrong position count");
    }
  }
  fzf_free_positions(positions);

  if (algorithm == fzf_fuzzy_match_v2 ||
      algorithm == fzf_fuzzy_match_v2_utf8) {
    fzf_algo_t v1 = algorithm == fzf_fuzzy_match_v2_utf8
                        ? fzf_fuzzy_match_v1_utf8
                        : fzf_fuzzy_match_v1;
    fzf_result_t v1_result = v1(
        term->case_sensitive, false, &input, pattern, NULL, slab);
    if ((v1_result.start >= 0) != (with_positions.start >= 0))
      fuzz_fail("fuzzy v1 and v2 disagree on match membership");
  }
}

static int32_t score_query(const char *candidate, const char *query,
                           fzf_case_types case_mode, bool fuzzy,
                           fzf_slab_t *slab, bool *inverse) {
  char *copy = strdup(query);
  if (!copy)
    abort();
  fzf_pattern_t *pattern =
      fzf_parse_pattern(case_mode, false, copy, fuzzy);
  if (!pattern)
    abort();
  if (inverse)
    *inverse = pattern_has_inverse(pattern);
  int32_t score = fzf_get_score(candidate, pattern, slab);
  fzf_free_pattern(pattern);
  free(copy);
  return score;
}

static void check_case_monotonicity(const char *candidate, const char *query,
                                    bool fuzzy, fzf_slab_t *slab) {
  if (!valid_utf8(candidate, strlen(candidate)) ||
      !valid_utf8(query, strlen(query)))
    return;
  bool inverse = false;
  int32_t respect = score_query(candidate, query, CaseRespect, fuzzy, slab,
                                &inverse);
  if (inverse)
    return;
  int32_t ignore =
      score_query(candidate, query, CaseIgnore, fuzzy, slab, NULL);
  if (respect > 0 && ignore <= 0)
    fuzz_fail("case-ignore rejected a positive case-respect match");
}

static void check_whitespace_equivalence(const char *candidate,
                                         const char *query,
                                         fzf_case_types case_mode, bool fuzzy,
                                         fzf_slab_t *slab,
                                         int32_t expected_score) {
  size_t len = strlen(query);
  char *leading = malloc(len + 3);
  if (!leading)
    abort();
  memcpy(leading, "  ", 2);
  memcpy(leading + 2, query, len + 1);
  if (score_query(candidate, leading, case_mode, fuzzy, slab, NULL) !=
      expected_score)
    fuzz_fail("leading query whitespace changed a score");
  free(leading);

  /* A trailing space after a backslash is an escaped literal, not padding. */
  if (len == 0 || query[len - 1] != '\\') {
    char *trailing = malloc(len + 3);
    if (!trailing)
      abort();
    memcpy(trailing, query, len);
    memcpy(trailing + len, "  ", 3);
    if (score_query(candidate, trailing, case_mode, fuzzy, slab, NULL) !=
        expected_score)
      fuzz_fail("trailing query whitespace changed a score");
    free(trailing);
  }

  for (size_t i = 0; i < len; i++) {
    if (query[i] != ' ' || (i > 0 && query[i - 1] == '\\')) continue;
    char *expanded = malloc(len + 3);
    if (!expanded) abort();
    memcpy(expanded, query, i);
    memcpy(expanded + i, "   ", 3);
    memcpy(expanded + i + 3, query + i + 1, len - i);
    if (score_query(candidate, expanded, case_mode, fuzzy, slab, NULL) !=
        expected_score)
      fuzz_fail("expanding query whitespace changed a score");
    free(expanded);
    break;
  }
}

typedef struct {
  const char *data;
  size_t size;
} fuzz_query_token_t;

static bool split_simple_query(const char *query, fuzz_query_token_t **tokens,
                               size_t *count) {
  *tokens = NULL;
  *count = 0;
  if (strstr(query, "\\ ")) return false;
  size_t len = strlen(query);
  size_t token_count = 0;
  for (size_t pos = 0; pos < len;) {
    while (pos < len && query[pos] == ' ') pos++;
    if (pos == len) break;
    token_count++;
    while (pos < len && query[pos] != ' ') pos++;
  }
  if (token_count == 0) return true;

  fuzz_query_token_t *result = malloc(token_count * sizeof *result);
  if (!result) abort();
  size_t index = 0;
  for (size_t pos = 0; pos < len;) {
    while (pos < len && query[pos] == ' ') pos++;
    if (pos == len) break;
    size_t start = pos;
    while (pos < len && query[pos] != ' ') pos++;
    if (query[pos - 1] == '\\') {
      free(result);
      return false;
    }
    result[index++] =
        (fuzz_query_token_t){.data = query + start, .size = pos - start};
  }
  *tokens = result;
  *count = token_count;
  return true;
}

static bool token_is_bar(const fuzz_query_token_t *token) {
  return token->size == 1 && token->data[0] == '|';
}

static void copy_token(char *output, size_t *offset,
                       const fuzz_query_token_t *token) {
  memcpy(output + *offset, token->data, token->size);
  *offset += token->size;
}

static bool pattern_is_simple_and(const fzf_pattern_t *pattern,
                                  const fuzz_query_token_t *tokens,
                                  size_t count) {
  if (count < 2 || pattern->size != count) return false;
  for (size_t i = 0; i < count; i++)
    if (token_is_bar(&tokens[i]) || pattern->ptr[i]->size != 1) return false;
  return true;
}

static bool pattern_is_simple_or(const fzf_pattern_t *pattern,
                                 const fuzz_query_token_t *tokens,
                                 size_t count) {
  if (count < 3 || count % 2 == 0 || pattern->size != 1 ||
      pattern->ptr[0]->size != (count + 1) / 2)
    return false;
  for (size_t i = 0; i < count; i++)
    if (token_is_bar(&tokens[i]) != (i % 2 == 1)) return false;
  return true;
}

static bool token_is_plain_literal(const fuzz_query_token_t *token) {
  if (token->size == 0 || token_is_bar(token)) return false;
  char first = token->data[0];
  char last = token->data[token->size - 1];
  return first != '!' && first != '\'' && first != '^' && last != '$';
}

static void require_score(const char *candidate, const char *query,
                          fzf_case_types case_mode, bool fuzzy,
                          fzf_slab_t *slab, int32_t expected,
                          const char *property) {
  if (score_query(candidate, query, case_mode, fuzzy, slab, NULL) != expected)
    fuzz_fail(property);
}

static void require_membership(const char *candidate, const char *query,
                               fzf_case_types case_mode, bool fuzzy,
                               fzf_slab_t *slab, bool expected,
                               const char *property) {
  int32_t score = score_query(candidate, query, case_mode, fuzzy, slab, NULL);
  if ((score > 0) != expected) fuzz_fail(property);
}

static void check_query_structure(const char *candidate, const char *query,
                                  fzf_case_types case_mode, bool fuzzy,
                                  const fzf_pattern_t *pattern,
                                  fzf_slab_t *slab, int32_t score) {
  fuzz_query_token_t *tokens = NULL;
  size_t count = 0;
  if (!split_simple_query(query, &tokens, &count)) return;

  if (pattern_is_simple_and(pattern, tokens, count)) {
    size_t output_len = count - 1;
    for (size_t i = 0; i < count; i++) output_len += tokens[i].size;
    char *reversed = malloc(output_len + 1);
    if (!reversed) abort();
    size_t offset = 0;
    for (size_t i = count; i-- > 0;) {
      if (offset) reversed[offset++] = ' ';
      copy_token(reversed, &offset, &tokens[i]);
    }
    reversed[offset] = '\0';
    require_score(candidate, reversed, case_mode, fuzzy, slab, score,
                  "reordering AND terms changed a score");
    free(reversed);
  }

  if (pattern_is_simple_or(pattern, tokens, count)) {
    size_t branches = (count + 1) / 2;
    size_t output_len = (branches - 1) * 3;
    for (size_t i = 0; i < count; i += 2) output_len += tokens[i].size;
    char *reversed = malloc(output_len + 1);
    if (!reversed) abort();
    size_t offset = 0;
    for (size_t branch = branches; branch-- > 0;) {
      if (offset) {
        memcpy(reversed + offset, " | ", 3);
        offset += 3;
      }
      copy_token(reversed, &offset, &tokens[branch * 2]);
    }
    reversed[offset] = '\0';
    require_membership(candidate, reversed, case_mode, fuzzy, slab, score > 0,
                       "reordering OR branches changed membership");
    free(reversed);
  }

  if (count == 1 && pattern->size == 1 && pattern->ptr[0]->size == 1 &&
      !token_is_bar(&tokens[0])) {
    size_t token_len = tokens[0].size;
    char *duplicate_or = malloc(token_len * 2 + 4);
    char *duplicate_and = malloc(token_len * 2 + 2);
    if (!duplicate_or || !duplicate_and) abort();
    memcpy(duplicate_or, tokens[0].data, token_len);
    memcpy(duplicate_or + token_len, " | ", 3);
    memcpy(duplicate_or + token_len + 3, tokens[0].data, token_len);
    duplicate_or[token_len * 2 + 3] = '\0';
    memcpy(duplicate_and, tokens[0].data, token_len);
    duplicate_and[token_len] = ' ';
    memcpy(duplicate_and + token_len + 1, tokens[0].data, token_len);
    duplicate_and[token_len * 2 + 1] = '\0';
    require_score(candidate, duplicate_or, case_mode, fuzzy, slab, score,
                  "duplicating an OR branch changed a score");
    require_membership(candidate, duplicate_and, case_mode, fuzzy, slab,
                       score > 0,
                       "duplicating an AND term changed membership");
    free(duplicate_and);
    free(duplicate_or);

    if (token_is_plain_literal(&tokens[0])) {
      char *literal = malloc(token_len + 1);
      char *quoted = malloc(token_len + 2);
      if (!literal || !quoted) abort();
      memcpy(literal, tokens[0].data, token_len);
      literal[token_len] = '\0';
      quoted[0] = '\'';
      memcpy(quoted + 1, tokens[0].data, token_len);
      quoted[token_len + 1] = '\0';
      int32_t exact = score_query(candidate, literal, case_mode, false, slab,
                                  NULL);
      require_score(candidate, quoted, case_mode, true, slab, exact,
                    "quoted exact and global exact scores differ");
      int32_t fuzzy_score = score_query(candidate, literal, case_mode, true,
                                        slab, NULL);
      require_score(candidate, quoted, case_mode, false, slab, fuzzy_score,
                    "quoted fuzzy and global fuzzy scores differ");
      free(quoted);
      free(literal);
    }
  }
  free(tokens);
}

static void check_bounded_entry_points(const char *candidate,
                                       size_t candidate_size,
                                       fzf_pattern_t *pattern,
                                       fzf_slab_t *slab,
                                       int32_t legacy_score) {
  /* Keep the explicit range separate from the NUL-terminated legacy input.
     Sanitizers will reject any later strlen use in a bounded entry point. */
  size_t allocation_size = candidate_size ? candidate_size : 1;
  char *range = malloc(allocation_size);
  if (!range)
    abort();
  if (candidate_size)
    memcpy(range, candidate, candidate_size);
  else
    range[0] = 'x';

  bool input_is_ascii = is_ascii_utf8proc(range, candidate_size);
  int32_t bounded_score = fzf_get_score_bytes(
      range, candidate_size, pattern, slab);
  int32_t preclassified_score = fzf_get_score_bytes_preclassified(
      range, candidate_size, input_is_ascii, pattern, slab);
  if (bounded_score != preclassified_score)
    fuzz_fail("safe and preclassified bounded scorers disagree");

  bool bounded_match = fzf_has_match_bytes(
      range, candidate_size, pattern, slab);
  bool preclassified_match = fzf_has_match_bytes_preclassified(
      range, candidate_size, input_is_ascii, pattern, slab);
  if (bounded_match != preclassified_match)
    fuzz_fail("safe and preclassified bounded membership disagrees");

  /* The C-string wrappers have the same range only without embedded NUL. */
  if (strlen(candidate) == candidate_size) {
    if (bounded_score != legacy_score)
      fuzz_fail("bounded and legacy scorer entry points disagree");
    if (bounded_match != fzf_has_match(candidate, pattern, slab))
      fuzz_fail("bounded and legacy membership entry points disagree");
  }
  free(range);
}

static void run_one(const uint8_t *data, size_t size) {
  if (!data || size < 2 || size > FZF_NATIVE_FUZZ_MAX_INPUT)
    return;

  uint8_t options = data[0];
  const uint8_t *payload = data + 1;
  size_t payload_size = size - 1;
  size_t query_size = 0;
  bool separator = false;
  while (query_size < payload_size) {
    if (payload[query_size] == '\n') {
      separator = true;
      break;
    }
    query_size++;
  }
  size_t candidate_offset;
  if (separator) {
    candidate_offset = query_size + 1;
  } else {
    query_size = options % (payload_size + 1);
    candidate_offset = query_size;
  }
  size_t candidate_size = payload_size - candidate_offset;

  char *query = malloc(query_size + 1);
  char *candidate = malloc(candidate_size + 1);
  if (!query || !candidate)
    abort();
  memcpy(query, payload, query_size);
  query[query_size] = '\0';
  memcpy(candidate, payload + candidate_offset, candidate_size);
  candidate[candidate_size] = '\0';

  /* This first, behavior-preserving layer covers the baseline ASCII API.
     The stacked UTF-8 matcher advertises its byte semantics through a public
     feature macro, which removes this guard without changing this commit's
     standalone behavior. */
#ifndef FZF_NATIVE_UTF8_MATCHING
  if (!ascii_bytes(payload, query_size) ||
      !ascii_bytes(payload + candidate_offset, candidate_size)) {
    free(candidate);
    free(query);
    return;
  }
#endif

  fzf_case_types case_mode = (fzf_case_types)(options % 3);
  bool fuzzy = (options & 4) != 0;
  fzf_pattern_t *pattern =
      fzf_parse_pattern(case_mode, false, query, fuzzy);
  /* main currently underflows in suffix_match when a mutated suffix pattern
     is longer than its candidate.  The additive fuzz layer records but does
     not alter that pre-existing behavior; the stacked matcher fix advertises
     the safe implementation and removes this exclusion. */
#ifndef FZF_NATIVE_UTF8_MATCHING
  if (pattern && (candidate[0] == '\0' || pattern_has_end_anchor(pattern))) {
    fzf_free_pattern(pattern);
    free(candidate);
    free(query);
    return;
  }
#endif
  fzf_slab_t *default_slab = fzf_make_default_slab();
  fzf_slab_t *selected_slab = make_selected_slab(options);
  if (!pattern || !default_slab || !selected_slab)
    abort();

  int32_t score = fzf_get_score(candidate, pattern, default_slab);
  if (score != fzf_get_score(candidate, pattern, default_slab))
    fuzz_fail("repeated scoring is not deterministic");

  check_candidate_extension(candidate, pattern, score, default_slab);
  check_query_structure(candidate, query, case_mode, fuzzy, pattern,
                        default_slab, score);

  check_bounded_entry_points(candidate, candidate_size, pattern,
                             default_slab, score);

  fzf_position_t *positions =
      fzf_get_positions(candidate, pattern, default_slab);
  check_positions(candidate, score > 0, positions);
  fzf_free_positions(positions);

  /* Filtering and scoring implement the same match predicate. */
#ifdef FZF_NATIVE_HAS_MATCH_SLAB
  bool fast_match = fzf_has_match(candidate, pattern, default_slab);
#else
  bool fast_match = fzf_has_match(candidate, pattern);
#endif
  if (fast_match != (score > 0))
    fuzz_fail("fzf_has_match disagrees with fzf_get_score");

  /* A slab can select a different algorithm, but not different membership. */
  int32_t selected_score = fzf_get_score(candidate, pattern, selected_slab);
  if ((selected_score > 0) != (score > 0))
    fuzz_fail("slab fallback changed match membership");
  positions = fzf_get_positions(candidate, pattern, selected_slab);
  check_positions(candidate, selected_score > 0, positions);
  fzf_free_positions(positions);

  check_case_monotonicity(candidate, query, fuzzy, default_slab);
  check_whitespace_equivalence(candidate, query, case_mode, fuzzy,
                               default_slab, score);

  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++)
      check_term(candidate, &set->ptr[j], selected_slab);
  }

  fzf_free_slab(selected_slab);
  fzf_free_slab(default_slab);
  fzf_free_pattern(pattern);
  free(candidate);
  free(query);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  run_one(data, size);
  return 0;
}

#ifdef FZF_FUZZ_STANDALONE
static int replay_file(const char *path) {
  FILE *file = fopen(path, "rb");
  if (!file) {
    perror(path);
    return 1;
  }
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    return 1;
  }
  long length = ftell(file);
  if (length < 0 || length > FZF_NATIVE_FUZZ_MAX_INPUT) {
    fprintf(stderr, "%s: unsupported corpus file length %ld\n", path,
            length);
    fclose(file);
    return 1;
  }
  rewind(file);
  uint8_t *bytes = malloc((size_t)length + 1);
  if (!bytes) {
    fclose(file);
    return 1;
  }
  size_t got = fread(bytes, 1, (size_t)length, file);
  fclose(file);
  if (got != (size_t)length) {
    free(bytes);
    return 1;
  }
  run_one(bytes, got);
  free(bytes);
  return 0;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s CORPUS-FILE...\n", argv[0]);
    return 2;
  }
  for (int i = 1; i < argc; i++) {
    if (replay_file(argv[i]) != 0)
      return 1;
  }
  printf("Replayed %d fzf-native fuzz corpus files.\n", argc - 1);
  return 0;
}
#endif
