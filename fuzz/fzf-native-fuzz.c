/* SPDX-License-Identifier: GPL-3.0-or-later
 * Coverage-guided fuzzer for fzf-native's pure-C matcher.
 *
 * The first input byte selects case/fuzzy/slab options.  The remaining bytes
 * are split at the first newline into QUERY and CANDIDATE.  If there is no
 * newline, the option byte also selects a split point.  This deliberately
 * keeps the format simple: a mutator can alter query syntax, UTF-8 bytes, and
 * matcher options without first satisfying a checksum or nested structure.
 *
 * Build with libFuzzer via `make fuzz-build`, or as a deterministic corpus
 * replay executable via `make fuzz-replay-build`.
 */

#include "fzf.h"
#include "fzf-additions.h"
#include "utf8_char_index.h"

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
  size_t off = 0;
  while (off < len) {
    utf8proc_int32_t cp;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)text + off, (utf8proc_ssize_t)(len - off),
        &cp);
    if (width <= 0)
      return false;
    off += (size_t)width;
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

static void check_positions(const char *source, const char *candidate,
                            bool matched, fzf_position_t *positions) {
  if (!matched && positions != NULL && positions->size != 0) {
    fprintf(stderr, "%s returned %zu positions for a failed match\n", source,
            positions->size);
    fuzz_fail("a non-match returned highlight positions");
  }
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
    fuzz_fail("a term returned duplicate highlight positions");
  bool increasing = positions->data[1] > positions->data[0];
  for (size_t i = 2; i < positions->size; i++) {
    if (positions->data[i] == positions->data[i - 1] ||
        ((positions->data[i] > positions->data[i - 1]) != increasing))
      fuzz_fail("a term returned unordered highlight positions");
  }
}

static fzf_algo_t utf8_variant(fzf_algo_t algo) {
  if (algo == fzf_fuzzy_match_v2)
    return fzf_fuzzy_match_v2_utf8;
  if (algo == fzf_fuzzy_match_v1)
    return fzf_fuzzy_match_v1_utf8;
  if (algo == fzf_exact_match_naive)
    return fzf_exact_match_utf8;
  if (algo == fzf_prefix_match)
    return fzf_prefix_match_utf8;
  if (algo == fzf_suffix_match)
    return fzf_suffix_match_utf8;
  if (algo == fzf_equal_match)
    return fzf_equal_match_utf8;
  return algo;
}

static const char *algo_name(fzf_algo_t algo) {
  if (algo == fzf_fuzzy_match_v2)
    return "fuzzy-v2";
  if (algo == fzf_fuzzy_match_v2_utf8)
    return "fuzzy-v2-utf8";
  if (algo == fzf_fuzzy_match_v1)
    return "fuzzy-v1";
  if (algo == fzf_fuzzy_match_v1_utf8)
    return "fuzzy-v1-utf8";
  if (algo == fzf_exact_match_naive)
    return "exact";
  if (algo == fzf_exact_match_utf8)
    return "exact-utf8";
  if (algo == fzf_prefix_match)
    return "prefix";
  if (algo == fzf_prefix_match_utf8)
    return "prefix-utf8";
  if (algo == fzf_suffix_match)
    return "suffix";
  if (algo == fzf_suffix_match_utf8)
    return "suffix-utf8";
  if (algo == fzf_equal_match)
    return "equal";
  if (algo == fzf_equal_match_utf8)
    return "equal-utf8";
  return "unknown";
}

static void check_term_positions(const char *candidate, const fzf_term_t *term,
                                 fzf_slab_t *slab) {
  if (!term->fn || !term->text)
    return;

  fzf_string_t input = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t *pattern = (fzf_string_t *)term->text;
  fzf_algo_t algo = term->fn;
  if (!is_ascii_utf8proc(input.data, input.size))
    algo = utf8_variant(algo);

  fzf_result_t without_positions =
      algo(term->case_sensitive, false, &input, pattern, NULL, slab);
  fzf_position_t *positions = fzf_pos_array(0);
  fzf_result_t with_positions =
      algo(term->case_sensitive, false, &input, pattern, positions, slab);

  /* Fuzzy v2 backtracks to its true start only when positions are requested,
     so START may differ.  Membership and score must remain observation-safe
     for every matcher. */
  if ((with_positions.start >= 0) != (without_positions.start >= 0) ||
      with_positions.score != without_positions.score) {
    fprintf(stderr,
            "without positions=(%d,%d,%d), with positions=(%d,%d,%d)\n",
            without_positions.start, without_positions.end,
            without_positions.score, with_positions.start,
            with_positions.end, with_positions.score);
    fuzz_fail("requesting positions changed a term result");
  }

  bool matched = with_positions.start >= 0;
  check_positions("direct term matcher", candidate, matched, positions);
  check_position_order(positions);
  /* A malformed byte string has no stable character-count oracle.  It still
     exercises the matcher under ASan/UBSan and the bounds check above treats
     each byte as the conservative limit, but require exact position counts
     only when both sides have an unambiguous UTF-8 character sequence. */
  if (matched && valid_utf8(input.data, input.size) &&
      valid_utf8(pattern->data, pattern->size)) {
    size_t pattern_chars = utf8_strlen(pattern->data, pattern->size);
    if (positions->size != pattern_chars) {
      fprintf(stderr,
              "%s matched %zu-byte candidate with %zu-byte term but returned "
              "%zu positions for %zu characters\n",
              algo_name(algo), input.size, pattern->size, positions->size,
              pattern_chars);
      fuzz_fail("a matched term returned the wrong number of positions");
    }
  }
  fzf_free_positions(positions);
}

static bool pattern_has_inverse(const fzf_pattern_t *pattern) {
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++)
      if (set->ptr[j].inv)
        return true;
  }
  return false;
}

static bool extension_preserves_term(const fzf_term_t *term, bool prepend) {
  if (term->inv || !term->fn || !term->text)
    return false;

  fzf_string_t *text = (fzf_string_t *)term->text;
  if (!valid_utf8(text->data, text->size))
    return false;

  if (term->fn == fzf_fuzzy_match_v1 ||
      term->fn == fzf_fuzzy_match_v1_utf8 ||
      term->fn == fzf_fuzzy_match_v2 ||
      term->fn == fzf_fuzzy_match_v2_utf8 ||
      term->fn == fzf_exact_match_naive ||
      term->fn == fzf_exact_match_utf8)
    return true;

  return prepend
             ? term->fn == fzf_suffix_match ||
                   term->fn == fzf_suffix_match_utf8
             : term->fn == fzf_prefix_match ||
                   term->fn == fzf_prefix_match_utf8;
}

static bool extension_preserves_pattern(fzf_pattern_t *pattern,
                                        bool prepend) {
  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++)
      if (!extension_preserves_term(&set->ptr[j], prepend))
        return false;
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
        utf8proc_int32_t cp;
        utf8proc_ssize_t width = utf8proc_iterate(
            (const utf8proc_uint8_t *)text->data + offset,
            (utf8proc_ssize_t)(text->size - offset), &cp);
        if (width <= 0)
          return true;
        if (cp == wanted)
          return true;
        offset += (size_t)width;
      }
    }
  }
  return false;
}

static bool term_extension_keeps_slab_path(const fzf_term_t *term,
                                           size_t candidate_units,
                                           size_t extended_units,
                                           const fzf_slab_t *slab) {
  if (!slab || (term->fn != fzf_fuzzy_match_v2 &&
                term->fn != fzf_fuzzy_match_v2_utf8))
    return true;

  const fzf_string_t *text = (const fzf_string_t *)term->text;
  size_t pattern_units = utf8_strlen(text->data, text->size);
  bool candidate_falls_back =
      candidate_units != 0 &&
      pattern_units > slab->I16.cap / candidate_units;
  bool extended_falls_back =
      extended_units != 0 &&
      pattern_units > slab->I16.cap / extended_units;
  return candidate_falls_back == extended_falls_back;
}

static bool extension_keeps_slab_path(const fzf_pattern_t *pattern,
                                      size_t candidate_units,
                                      size_t extended_units,
                                      const fzf_slab_t *slab) {
  if (!slab)
    return true;

  for (size_t i = 0; i < pattern->size; i++) {
    const fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++) {
      const fzf_term_t *term = &set->ptr[j];
      if (!term_extension_keeps_slab_path(term, candidate_units,
                                          extended_units, slab))
        return false;
    }
  }
  return true;
}

static void check_ascii_utf8_term_score(const char *candidate,
                                        const fzf_term_t *term,
                                        fzf_slab_t *slab) {
  static const char extension[] = "\xf4\x8f\xbf\xbf";
  size_t candidate_len = strlen(candidate);
  const fzf_string_t *pattern = (const fzf_string_t *)term->text;
  if (!extension_preserves_term(term, false) ||
      !is_ascii_utf8proc(candidate, candidate_len) ||
      !valid_utf8(pattern->data, pattern->size) ||
      !term_extension_keeps_slab_path(term, candidate_len,
                                      candidate_len + 1, slab))
    return;

  size_t offset = 0;
  while (offset < pattern->size) {
    utf8proc_int32_t cp;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)pattern->data + offset,
        (utf8proc_ssize_t)(pattern->size - offset), &cp);
    if (width <= 0 || cp == 0x10ffff) return;
    offset += (size_t)width;
  }

  char *extended = malloc(candidate_len + sizeof(extension));
  if (!extended) abort();
  memcpy(extended, candidate, candidate_len);
  memcpy(extended + candidate_len, extension, sizeof(extension));

  fzf_string_t ascii_input = {.data = candidate, .size = candidate_len};
  fzf_string_t utf8_input = {
      .data = extended, .size = candidate_len + sizeof(extension) - 1};
  fzf_algo_t ascii_algo = term->fn;
  fzf_algo_t utf8_algo = utf8_variant(ascii_algo);
  fzf_result_t ascii_result =
      ascii_algo(term->case_sensitive, false, &ascii_input,
                 (fzf_string_t *)pattern, NULL, slab);
  fzf_result_t utf8_result =
      utf8_algo(term->case_sensitive, false, &utf8_input,
                (fzf_string_t *)pattern, NULL, slab);
  if (ascii_result.score != utf8_result.score) {
    fprintf(stderr, "%s_score=%d %s_score=%d\n", algo_name(ascii_algo),
            ascii_result.score, algo_name(utf8_algo), utf8_result.score);
    fuzz_fail("ASCII-to-UTF-8 dispatch changed a term score");
  }
  free(extended);
}

static void check_candidate_extension_monotonicity(
    const char *candidate, fzf_pattern_t *pattern, int32_t score,
    fzf_slab_t *slab) {
  static const char *extensions[] = {"x", " ", "\xc3\xa9",
                                     "\xf0\x9f\x9a\x80"};
  size_t len = strlen(candidate);
  if (score <= 0 || !valid_utf8(candidate, len))
    return;

  char *extended = malloc(len + 5);
  if (!extended)
    abort();

  /* A positive fuzzy, substring-exact, or prefix match remains a match when
     text is appended.  The symmetric relation holds for suffix-safe terms
     when text is prepended.  Equal and inverse terms are excluded above.
     Non-ASCII extensions also force an ASCII candidate through the UTF-8
     algorithm variants, checking that dispatch cannot change membership. */
  bool append_safe = extension_preserves_pattern(pattern, false);
  bool prepend_safe = extension_preserves_pattern(pattern, true);
  for (size_t i = 0; i < sizeof(extensions) / sizeof(extensions[0]); i++) {
    const char *extension = extensions[i];
    size_t extension_len = strlen(extension);

    if (append_safe) {
      memcpy(extended, candidate, len);
      memcpy(extended + len, extension, extension_len + 1);
      if (fzf_get_score(extended, pattern, slab) <= 0) {
        fprintf(stderr, "extension=%zu direction=append\n", i);
        fuzz_fail("appending text destroyed a prefix-safe match");
      }
    }

    if (prepend_safe) {
      memcpy(extended, extension, extension_len);
      memcpy(extended + extension_len, candidate, len + 1);
      if (fzf_get_score(extended, pattern, slab) <= 0) {
        fprintf(stderr, "extension=%zu direction=prepend\n", i);
        fuzz_fail("prepending text destroyed a suffix-safe match");
      }
    }
  }

  /* Appending a non-matching scalar cannot change a prefix-safe score.  On
     an ASCII candidate this deliberately switches dispatch to the UTF-8
     matcher and compares its score with the byte matcher's score. */
  static const char score_extension[] = "\xf4\x8f\xbf\xbf";
  if (append_safe && is_ascii_utf8proc(candidate, len) &&
      !pattern_contains_codepoint(pattern, 0x10ffff) &&
      extension_keeps_slab_path(pattern, len, len + 1, slab)) {
    memcpy(extended, candidate, len);
    memcpy(extended + len, score_extension, sizeof(score_extension));
    int32_t extended_score = fzf_get_score(extended, pattern, slab);
    if (extended_score != score) {
      fprintf(stderr, "ascii_score=%d utf8_score=%d\n", score,
              extended_score);
      fuzz_fail("ASCII-to-UTF-8 dispatch changed a prefix-safe score");
    }
  }

  free(extended);
}

static void check_case_monotonicity(const char *candidate, const char *query,
                                    bool fuzzy, fzf_slab_t *slab) {
  size_t candidate_len = strlen(candidate);
  size_t query_len = strlen(query);
  if (!valid_utf8(candidate, candidate_len) || !valid_utf8(query, query_len))
    return;

  char *respect_query = strdup(query);
  char *ignore_query = strdup(query);
  if (!respect_query || !ignore_query)
    abort();
  fzf_pattern_t *respect =
      fzf_parse_pattern(CaseRespect, false, respect_query, fuzzy);
  fzf_pattern_t *ignore =
      fzf_parse_pattern(CaseIgnore, false, ignore_query, fuzzy);
  if (!respect || !ignore)
    abort();

  /* Negation reverses this relation: !A can accept "a" in respect mode and
     reject it in ignore mode.  With positive terms only, relaxing case can
     add matches but must never remove one. */
  if (!pattern_has_inverse(respect)) {
    int32_t respect_score = fzf_get_score(candidate, respect, slab);
    int32_t ignore_score = fzf_get_score(candidate, ignore, slab);
    if (respect_score > 0 && ignore_score <= 0) {
      fprintf(stderr, "respect_score=%d ignore_score=%d fuzzy=%d\n",
              respect_score, ignore_score, (int)fuzzy);
      fuzz_fail("case-ignore rejected a positive-only case-respect match");
    }
  }

  fzf_free_pattern(ignore);
  fzf_free_pattern(respect);
  free(ignore_query);
  free(respect_query);
}

typedef struct {
  const char *data;
  size_t size;
} fuzz_query_token_t;

static int32_t score_query(const char *candidate, const char *query,
                           fzf_case_types case_mode, bool fuzzy,
                           fzf_slab_t *slab) {
  char *mutable_query = strdup(query);
  if (!mutable_query) abort();
  fzf_pattern_t *pattern =
      fzf_parse_pattern(case_mode, false, mutable_query, fuzzy);
  if (!pattern) abort();
  int32_t score = fzf_get_score(candidate, pattern, slab);
  fzf_free_pattern(pattern);
  free(mutable_query);
  return score;
}

static void check_equivalent_query_score(const char *candidate,
                                         const char *query,
                                         fzf_case_types case_mode,
                                         bool fuzzy, fzf_slab_t *slab,
                                         int32_t expected,
                                         const char *property) {
  int32_t actual = score_query(candidate, query, case_mode, fuzzy, slab);
  if (actual != expected) {
    fprintf(stderr, "expected_score=%d rewritten_score=%d fuzzy=%d\n",
            expected, actual, (int)fuzzy);
    fuzz_fail(property);
  }
}

static void check_equivalent_query_membership(const char *candidate,
                                              const char *query,
                                              fzf_case_types case_mode,
                                              bool fuzzy, fzf_slab_t *slab,
                                              bool expected,
                                              const char *property) {
  int32_t actual = score_query(candidate, query, case_mode, fuzzy, slab);
  if ((actual > 0) != expected) {
    fprintf(stderr, "expected_match=%d rewritten_score=%d fuzzy=%d\n",
            (int)expected, actual, (int)fuzzy);
    fuzz_fail(property);
  }
}

/* Split the subset of extended-search syntax in which spaces are true token
   separators.  Escaped spaces and tokens ending in a backslash are rejected:
   moving the latter before a separator would create a new escaped space. */
static bool split_simple_query(const char *query, fuzz_query_token_t **tokens,
                               size_t *count) {
  *tokens = NULL;
  *count = 0;
  if (strstr(query, "\\ ") != NULL) return false;

  size_t len = strlen(query);
  size_t token_count = 0;
  for (size_t pos = 0; pos < len;) {
    while (pos < len && query[pos] == ' ') pos++;
    if (pos == len) break;
    token_count++;
    while (pos < len && query[pos] != ' ') pos++;
  }
  if (token_count == 0) return true;

  fuzz_query_token_t *result =
      malloc(token_count * sizeof(fuzz_query_token_t));
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

static bool query_token_is_bar(const fuzz_query_token_t *token) {
  return token->size == 1 && token->data[0] == '|';
}

static void append_query_token(char *output, size_t *offset,
                               const fuzz_query_token_t *token) {
  memcpy(output + *offset, token->data, token->size);
  *offset += token->size;
}

static bool pattern_is_simple_and(const fzf_pattern_t *pattern,
                                  const fuzz_query_token_t *tokens,
                                  size_t count) {
  if (count < 2 || pattern->size != count) return false;
  for (size_t i = 0; i < count; i++) {
    if (query_token_is_bar(&tokens[i]) || pattern->ptr[i]->size != 1)
      return false;
  }
  return true;
}

static bool pattern_is_simple_or(const fzf_pattern_t *pattern,
                                 const fuzz_query_token_t *tokens,
                                 size_t count) {
  if (count < 3 || count % 2 == 0 || pattern->size != 1 ||
      pattern->ptr[0]->size != (count + 1) / 2)
    return false;
  for (size_t i = 0; i < count; i++) {
    if (query_token_is_bar(&tokens[i]) != (i % 2 == 1)) return false;
  }
  return true;
}

static void check_query_whitespace_metamorphisms(
    const char *candidate, const char *query, fzf_case_types case_mode,
    bool fuzzy, fzf_slab_t *slab, int32_t score) {
  size_t len = strlen(query);

  char *leading = malloc(len + 3);
  if (!leading) abort();
  memcpy(leading, "  ", 2);
  memcpy(leading + 2, query, len + 1);
  check_equivalent_query_score(
      candidate, leading, case_mode, fuzzy, slab, score,
      "leading query whitespace changed a score");
  free(leading);

  /* A trailing space immediately after a backslash becomes an escaped literal
     in this parser.  Otherwise two extra spaces are trimmed back to QUERY,
     including when QUERY already ends in an escaped space. */
  if (len == 0 || query[len - 1] != '\\') {
    char *trailing = malloc(len + 3);
    if (!trailing) abort();
    memcpy(trailing, query, len);
    memcpy(trailing + len, "  ", 3);
    check_equivalent_query_score(
        candidate, trailing, case_mode, fuzzy, slab, score,
        "trailing query whitespace changed a score");
    free(trailing);
  }

  for (size_t i = 0; i < len; i++) {
    if (query[i] != ' ' || (i > 0 && query[i - 1] == '\\')) continue;
    char *expanded = malloc(len + 3);
    if (!expanded) abort();
    memcpy(expanded, query, i);
    memcpy(expanded + i, "   ", 3);
    memcpy(expanded + i + 3, query + i + 1, len - i);
    check_equivalent_query_score(
        candidate, expanded, case_mode, fuzzy, slab, score,
        "expanding query whitespace changed a score");
    free(expanded);
    break;
  }
}

static bool token_is_operator_free_literal(const fuzz_query_token_t *token) {
  if (token->size == 0 || query_token_is_bar(token)) return false;
  char first = token->data[0];
  char last = token->data[token->size - 1];
  return first != '!' && first != '\'' && first != '^' && last != '$';
}

static void check_query_structure_metamorphisms(
    const char *candidate, const char *query, fzf_case_types case_mode,
    bool fuzzy, const fzf_pattern_t *pattern, fzf_slab_t *slab,
    int32_t score) {
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
      if (offset > 0) reversed[offset++] = ' ';
      append_query_token(reversed, &offset, &tokens[i]);
    }
    reversed[offset] = '\0';
    check_equivalent_query_score(
        candidate, reversed, case_mode, fuzzy, slab, score,
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
      if (offset > 0) {
        memcpy(reversed + offset, " | ", 3);
        offset += 3;
      }
      append_query_token(reversed, &offset, &tokens[branch * 2]);
    }
    reversed[offset] = '\0';
    check_equivalent_query_membership(
        candidate, reversed, case_mode, fuzzy, slab, score > 0,
        "reordering OR branches changed match membership");
    free(reversed);
  }

  if (count == 1 && pattern->size == 1 && pattern->ptr[0]->size == 1 &&
      !query_token_is_bar(&tokens[0])) {
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

    check_equivalent_query_score(
        candidate, duplicate_or, case_mode, fuzzy, slab, score,
        "duplicating an OR branch changed a score");
    check_equivalent_query_membership(
        candidate, duplicate_and, case_mode, fuzzy, slab, score > 0,
        "duplicating an AND term changed match membership");
    free(duplicate_and);
    free(duplicate_or);

    if (token_is_operator_free_literal(&tokens[0])) {
      char *literal = malloc(token_len + 1);
      char *quoted = malloc(token_len + 2);
      if (!literal || !quoted) abort();
      memcpy(literal, tokens[0].data, token_len);
      literal[token_len] = '\0';
      quoted[0] = '\'';
      memcpy(quoted + 1, tokens[0].data, token_len);
      quoted[token_len + 1] = '\0';

      int32_t exact_score =
          score_query(candidate, literal, case_mode, false, slab);
      check_equivalent_query_score(
          candidate, quoted, case_mode, true, slab, exact_score,
          "quoted exact and global exact scores differ");
      int32_t fuzzy_score =
          score_query(candidate, literal, case_mode, true, slab);
      check_equivalent_query_score(
          candidate, quoted, case_mode, false, slab, fuzzy_score,
          "quoted fuzzy and global fuzzy scores differ");
      free(quoted);
      free(literal);
    }
  }

  free(tokens);
}

static void check_fuzzy_term_algorithms(const char *candidate,
                                        const fzf_term_t *term,
                                        fzf_slab_t *slab) {
  bool fuzzy_term = term->fn == fzf_fuzzy_match_v2 ||
                    term->fn == fzf_fuzzy_match_v2_utf8;
  if (!fuzzy_term || !term->text)
    return;

  fzf_string_t input = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t *pattern = (fzf_string_t *)term->text;

  /* Semantic comparisons on malformed UTF-8 do not have a stable oracle.
     The general scorer still receives those inputs so sanitizers cover them. */
  if (!valid_utf8(input.data, input.size) ||
      !valid_utf8(pattern->data, pattern->size))
    return;

  bool utf8 = !is_ascii_utf8proc(input.data, input.size) ||
              !is_ascii_utf8proc(pattern->data, pattern->size);
  fzf_algo_t v1 = utf8 ? fzf_fuzzy_match_v1_utf8 : fzf_fuzzy_match_v1;
  fzf_algo_t v2 = utf8 ? fzf_fuzzy_match_v2_utf8 : fzf_fuzzy_match_v2;

  /* Avoid turning a single fuzz iteration into an unbounded DP allocation.
     The selected slab still forces v2's documented v1 fallback at many
     smaller boundary combinations. */
  size_t text_units = utf8 ? utf8_strlen(input.data, input.size) : input.size;
  size_t pat_units = utf8 ? utf8_strlen(pattern->data, pattern->size)
                          : pattern->size;
  if (text_units != 0 && pat_units > 100 * 1024 / text_units)
    return;

  fzf_result_t r1 = v1(term->case_sensitive, false, &input, pattern, NULL, slab);
  fzf_result_t r2 = v2(term->case_sensitive, false, &input, pattern, NULL, slab);
  if ((r1.start >= 0) != (r2.start >= 0))
    fuzz_fail("fuzzy v1 and v2 disagree on match membership");
}

static void run_one(const uint8_t *data, size_t size) {
  if (!data || size < 2 || size > FZF_NATIVE_FUZZ_MAX_INPUT)
    return;

  uint8_t options = data[0];
  const uint8_t *payload = data + 1;
  size_t payload_size = size - 1;
  size_t query_size = 0;
  bool found_separator = false;
  for (; query_size < payload_size; query_size++) {
    if (payload[query_size] == '\n') {
      found_separator = true;
      break;
    }
  }
  size_t candidate_offset;
  if (found_separator) {
    candidate_offset = query_size + 1;
  } else {
    query_size = payload_size == 0 ? 0 : options % (payload_size + 1);
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

  fzf_case_types case_mode = (fzf_case_types)(options % 3);
  bool fuzzy = (options & 4) != 0;
  fzf_pattern_t *pattern =
      fzf_parse_pattern(case_mode, false, query, fuzzy);
  fzf_slab_t *slab = make_selected_slab(options);
  fzf_slab_t *default_slab = fzf_make_default_slab();
  if (!pattern || !slab || !default_slab)
    abort();

  check_case_monotonicity(candidate, query, fuzzy, default_slab);

  int32_t score = fzf_get_score(candidate, pattern, default_slab);
  int32_t repeated_score = fzf_get_score(candidate, pattern, default_slab);
  if (score != repeated_score)
    fuzz_fail("repeated scoring is not deterministic");

  check_query_whitespace_metamorphisms(candidate, query, case_mode, fuzzy,
                                       default_slab, score);
  check_query_structure_metamorphisms(candidate, query, case_mode, fuzzy,
                                      pattern, default_slab, score);

  check_candidate_extension_monotonicity(candidate, pattern, score,
                                         default_slab);

  bool fast_match = fzf_has_match(candidate, pattern, default_slab);
  if (fast_match != (score > 0)) {
    fprintf(stderr, "score=%d has_match=%d case=%d fuzzy=%d\n", score,
            (int)fast_match, (int)case_mode, (int)fuzzy);
    fuzz_fail("fzf_has_match disagrees with fzf_get_score");
  }

  int32_t fallback_score = fzf_get_score(candidate, pattern, slab);
  if ((fallback_score > 0) != (score > 0)) {
    fprintf(stderr, "default_score=%d fallback_score=%d selected_cap=%zu\n",
            score, fallback_score, slab->I16.cap);
    fuzz_fail("slab fallback changed match membership");
  }

  fzf_position_t *positions =
      fzf_get_positions(candidate, pattern, default_slab);
  check_positions("fzf_get_positions", candidate, score > 0, positions);
  fzf_free_positions(positions);

  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++) {
      check_term_positions(candidate, &set->ptr[j], slab);
      check_ascii_utf8_term_score(candidate, &set->ptr[j], slab);
      check_fuzzy_term_algorithms(candidate, &set->ptr[j], slab);
    }
  }

  fzf_free_slab(default_slab);
  fzf_free_slab(slab);
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
    fprintf(stderr, "%s: unsupported corpus file length %ld\n", path, length);
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
