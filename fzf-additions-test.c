/* SPDX-License-Identifier: GPL-3.0-or-later
 * Standalone C unit tests for fzf-additions.c (fzf_has_match).
 *
 * Build and run via `make ctest-additions` (or `make ctest`, which runs
 * every C-level test target).  No Emacs runtime; pure C against fzf.h
 * and fzf-additions.h.
 *
 * Each test verifies that fzf_has_match agrees with fzf_get_score on
 * the boolean question "does this pattern match?" — fzf_get_score
 * returns > 0 on match, 0 on no-match, so the contract is:
 *     fzf_has_match(t, p)  ==  (fzf_get_score(t, p, slab) > 0)
 * for all combinations of algorithm, prefix tokens, negation, OR/AND
 * composition, and case mode.  fzf-additions is the cheap fast path; if
 * it ever disagrees with the canonical scorer's match decision, the
 * filter set will diverge from what the user expects after the cap.
 */

#include "fzf.h"
#include "fzf-additions.h"
#include "fzf-private.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failed = 0;
#define CHECK(cond) do {                                                \
    if (!(cond)) {                                                      \
      fprintf(stderr, "  FAIL %s:%d: %s\n", __FILE__, __LINE__, #cond); \
      failed++;                                                         \
    }                                                                   \
  } while (0)

#define RUN(name) do { printf("RUN  %s\n", #name); name(); } while (0)

/* Parse PATTERN_STR (mutated then freed by fzf_parse_pattern), score
   TEXT against it, and assert fzf_has_match matches the score's match
   verdict.  MODE = fzf_case_types; FUZZY = true to enable v2 fuzzy. */
static void check_agreement(const char *label,
                            const char *text,
                            const char *pattern_str,
                            fzf_case_types mode, bool fuzzy,
                            bool expect_match) {
  char *dup = strdup(pattern_str);
  fzf_pattern_t *p = fzf_parse_pattern(mode, false, dup, fuzzy);
  fzf_slab_t *slab = fzf_make_default_slab();

  int32_t score = fzf_get_score(text, p, slab);
  bool    score_says = (score > 0);
  bool    addn_says  = fzf_has_match(text, p, slab);

  if (score_says != expect_match) {
    fprintf(stderr, "  NOTE %s: fzf_get_score disagrees with expectation "
                    "(text='%s' pattern='%s' score=%d expected=%d)\n",
            label, text, pattern_str, score, (int)expect_match);
  }
  if (addn_says != score_says) {
    fprintf(stderr, "  FAIL %s: fzf_has_match=%d != fzf_get_score>0 (%d) "
                    "(text='%s' pattern='%s')\n",
            label, (int)addn_says, (int)score_says, text, pattern_str);
    failed++;
  } else if (addn_says != expect_match) {
    /* Both disagree with expectation — surface but don't double-count. */
    fprintf(stderr, "  FAIL %s: both say %d, expected %d "
                    "(text='%s' pattern='%s')\n",
            label, (int)addn_says, (int)expect_match, text, pattern_str);
    failed++;
  }

  fzf_free_slab(slab);
  fzf_free_pattern(p);
  free(dup);
}

/* --- Tests --- */

static void test_fuzzy_basic_match(void) {
  check_agreement("fuzzy basic", "src/foo/bar.c", "fbc",
                  CaseIgnore, true, true);
}
static void test_fuzzy_basic_no_match(void) {
  check_agreement("fuzzy no-match", "src/foo/bar.c", "xyz",
                  CaseIgnore, true, false);
}
static void test_fuzzy_empty_pattern(void) {
  check_agreement("fuzzy empty", "anything", "",
                  CaseIgnore, true, true);
}
static void test_fuzzy_pattern_longer_than_text(void) {
  check_agreement("fuzzy pattern>text", "ab", "abcdef",
                  CaseIgnore, true, false);
}

static void test_exact_match(void) {
  check_agreement("exact 'pat", "foobarbaz", "'bar",
                  CaseIgnore, true, true);
}
static void test_exact_no_match(void) {
  check_agreement("exact 'pat miss", "foobarbaz", "'qux",
                  CaseIgnore, true, false);
}

static void test_prefix_match(void) {
  check_agreement("prefix ^pat", "fzf-native", "^fzf",
                  CaseIgnore, true, true);
}
static void test_prefix_no_match(void) {
  check_agreement("prefix ^pat miss", "fzf-native", "^native",
                  CaseIgnore, true, false);
}

static void test_suffix_match(void) {
  check_agreement("suffix pat$", "main.c", ".c$",
                  CaseIgnore, true, true);
}
static void test_suffix_no_match(void) {
  check_agreement("suffix pat$ miss", "main.c", ".h$",
                  CaseIgnore, true, false);
}

static void test_anchored_matches_trim_candidate_whitespace(void) {
  check_agreement("prefix trims leading whitespace", " \tfoo", "^foo",
                  CaseIgnore, true, true);
  check_agreement("suffix trims trailing whitespace", "foo.c \n", ".c$",
                  CaseIgnore, true, true);
  check_agreement("equal trims surrounding whitespace", " \tabc \n", "^abc$",
                  CaseIgnore, true, true);
  check_agreement("compound suffix trims newline", "src/foo/main.c\n",
                  "foo | bar !test .c$", CaseIgnore, true, true);
}

static void test_equal_match(void) {
  /* fzf produces fzf_equal_match only for ^...$ (prefix+suffix combo);
     'abc$ → exact substring (the `'` overrides the suffix anchor). */
  check_agreement("equal ^pat$", "abc", "^abc$",
                  CaseIgnore, true, true);
}
static void test_equal_no_match_different_string(void) {
  check_agreement("equal ^pat$ different", "abcd", "^abc$",
                  CaseIgnore, true, false);
}

static void test_equal_preserves_pattern_edge_whitespace(void) {
  fzf_string_t ascii_text = {.data = "- ", .size = 2};
  fzf_string_t ascii_pattern = {.data = "- ", .size = 2};
  fzf_result_t ascii = fzf_equal_match(
      false, false, &ascii_text, &ascii_pattern, NULL, NULL);
  CHECK(ascii.start == 0);
  CHECK(ascii.end == 2);
  CHECK(ascii.score > 0);

  fzf_string_t ascii_leading_text = {.data = " -", .size = 2};
  fzf_string_t ascii_leading_pattern = {.data = " -", .size = 2};
  fzf_result_t ascii_leading = fzf_equal_match(
      false, false, &ascii_leading_text, &ascii_leading_pattern, NULL, NULL);
  CHECK(ascii_leading.start == 0);
  CHECK(ascii_leading.end == 2);
  CHECK(ascii_leading.score > 0);

  fzf_string_t space_text = {.data = " ", .size = 1};
  fzf_string_t space_pattern = {.data = " ", .size = 1};
  fzf_result_t space = fzf_equal_match(
      false, false, &space_text, &space_pattern, NULL, NULL);
  CHECK(space.start == 0);
  CHECK(space.end == 1);
  CHECK(space.score > 0);

  fzf_string_t utf8_text = {.data = "你 ", .size = strlen("你 ")};
  fzf_string_t utf8_pattern = {.data = "你 ", .size = strlen("你 ")};
  fzf_result_t utf8 = fzf_equal_match_utf8(
      false, false, &utf8_text, &utf8_pattern, NULL, NULL);
  CHECK(utf8.start == 0);
  CHECK(utf8.end == 2);
  CHECK(utf8.score > 0);

  fzf_string_t utf8_leading_text = {
      .data = " 你", .size = strlen(" 你")};
  fzf_string_t utf8_leading_pattern = {
      .data = " 你", .size = strlen(" 你")};
  fzf_result_t utf8_leading = fzf_equal_match_utf8(
      false, false, &utf8_leading_text, &utf8_leading_pattern, NULL, NULL);
  CHECK(utf8_leading.start == 0);
  CHECK(utf8_leading.end == 2);
  CHECK(utf8_leading.score > 0);
}

static void test_parsed_equal_preserves_escaped_edge_whitespace(void) {
  /* Exercise the public parser and both fuzzy settings.  In either mode the
     two anchors select EqualMatch; escaped spaces belong to the term and must
     not be trimmed from the candidate before the cheap membership check. */
  check_agreement("fuzzy equal escaped leading space", " -", "^\\ -$",
                  CaseRespect, true, true);
  check_agreement("fuzzy equal escaped trailing space", "- ", "^-\\ $",
                  CaseRespect, true, true);
  check_agreement("fuzzy equal escaped single space", " ", "^\\ $",
                  CaseRespect, true, true);
  check_agreement("exact equal escaped leading space", " -", "^\\ -$",
                  CaseRespect, false, true);
  check_agreement("exact equal escaped trailing space", "- ", "^-\\ $",
                  CaseRespect, false, true);
  check_agreement("exact equal escaped single space", " ", "^\\ $",
                  CaseRespect, false, true);
}

static void test_negation_term_excludes(void) {
  /* "foo !bar" — must contain foo AND must NOT contain bar. */
  check_agreement("negation excludes", "src/foobar.c", "foo !bar",
                  CaseIgnore, true, false);
  check_agreement("negation passes",   "src/foo.c",    "foo !bar",
                  CaseIgnore, true, true);
}

static void test_and_across_term_sets(void) {
  check_agreement("AND both", "src/foo/bar.c", "foo bar",
                  CaseIgnore, true, true);
  check_agreement("AND missing", "src/foo.c", "foo bar",
                  CaseIgnore, true, false);
}

static void test_or_within_term_set(void) {
  check_agreement("OR a", "src/foo.c", "foo | bar",
                  CaseIgnore, true, true);
  check_agreement("OR b", "src/bar.c", "foo | bar",
                  CaseIgnore, true, true);
  check_agreement("OR neither", "src/baz.c", "foo | bar",
                  CaseIgnore, true, false);
}

static void test_or_satisfied_only_by_inverse_term(void) {
  check_agreement("OR inverse branch", "", "!Do | be|",
                  CaseRespect, false, true);
  check_agreement("OR inverse branch with text", "quux", "!foo | bar",
                  CaseIgnore, true, true);
}

static void test_small_slab_long_gap_preserves_match(void) {
  const char *text =
      "s........................................................................|";
  char *dup = strdup("s|");
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseRespect, false, dup, true);
  fzf_slab_t *large = fzf_make_default_slab();
  fzf_slab_t *small =
      fzf_make_slab((fzf_slab_config_t){64, 64});
  CHECK(fzf_get_score(text, pattern, large) > 0);
  CHECK(fzf_get_score(text, pattern, small) > 0);
  CHECK(fzf_has_match(text, pattern, small));
  fzf_free_slab(small);
  fzf_free_slab(large);
  fzf_free_pattern(pattern);
  free(dup);
}

static void test_small_slab_inverse_long_gap_preserves_membership(void) {
  const char *text = "nknnnnnnnnnnnnnnnnnnnnnnnnnnnnnk";
  char *dup = strdup("!'kk");
  /* With global exact matching disabled, a quote after `!' selects fuzzy
     matching for the inverse term.  The tiny slab forces v2 to fall back to
     v1, whose valid long-gap match has a non-positive raw score. */
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseSmart, false, dup, false);
  fzf_slab_t *large = fzf_make_default_slab();
  fzf_slab_t *small = fzf_make_slab((fzf_slab_config_t){1, 1});
  CHECK(fzf_get_score(text, pattern, large) == 0);
  CHECK(fzf_get_score(text, pattern, small) == 0);
  CHECK(!fzf_has_match(text, pattern, small));
  fzf_free_slab(small);
  fzf_free_slab(large);
  fzf_free_pattern(pattern);
  free(dup);
}

static void test_utf8_v1_reverse_scan_tightens_match(void) {
  char *dup = strdup("ab");
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, false, dup, true);
  /* Both candidates exceed this slab's v2 capacity and take the respective
     ASCII/UTF-8 v1 paths.  Appending a non-matching scalar must not change
     which `a' starts the shortest matching range. */
  fzf_slab_t *slab = fzf_make_slab((fzf_slab_config_t){1, 1});
  int32_t ascii_score = fzf_get_score("a---ab", pattern, slab);
  int32_t utf8_score = fzf_get_score("a---ab\xf4\x8f\xbf\xbf", pattern, slab);
  CHECK(ascii_score == 56);
  CHECK(utf8_score == ascii_score);
  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
  free(dup);
}

static void test_case_ignore(void) {
  check_agreement("case-ignore matches", "SrcFooBar", "srcfoo",
                  CaseIgnore, true, true);
}
static void test_case_respect_matches_when_case_aligns(void) {
  check_agreement("case-respect matches", "SrcFooBar", "Foo",
                  CaseRespect, true, true);
}
static void test_case_respect_no_match_when_case_differs(void) {
  check_agreement("case-respect rejects", "SrcFooBar", "FOO",
                  CaseRespect, true, false);
}
static void test_smart_case_lowercase_query_ignores_case(void) {
  check_agreement("smart all-lower", "SrcFooBar", "srcfoo",
                  CaseSmart, true, true);
}
static void test_smart_case_uppercase_query_respects_case(void) {
  check_agreement("smart has-upper", "SrcFooBar", "Foo",
                  CaseSmart, true, true);
  check_agreement("smart has-upper rejects mismatch", "srcfoo", "Foo",
                  CaseSmart, true, false);
}

/* Combined: AND + OR + negation in one pattern. */
static void test_compound_pattern(void) {
  /* (foo OR bar) AND NOT test AND .c$ */
  check_agreement("compound match",  "src/foo/main.c",     "foo | bar !test .c$",
                  CaseIgnore, true, true);
  check_agreement("compound !test reject", "src/foo/main_test.c", "foo | bar !test .c$",
                  CaseIgnore, true, false);
  check_agreement("compound suffix reject", "src/foo.py",         "foo | bar !test .c$",
                  CaseIgnore, true, false);
}

/* UTF-8 / non-ASCII terms.  fzf_parse_pattern routes these to the `_utf8'
   algorithm variants, which fzf_has_match cannot match byte-wise; it must
   defer to the full scorer.  These cases guard that deferral: if it regresses
   (e.g. the ASCII-only dispatch returns false for a `_utf8' term again),
   fzf_has_match disagrees with fzf_get_score>0 and check_agreement FAILS.
   They also independently oracle the match/no-match verdict for Greek,
   Cyrillic, CJK, Latin-diacritic case folding, and inverted UTF-8 terms. */
static void test_utf8_terms(void) {
  check_agreement("utf8 greek fuzzy",   "ελληνικά", "ελ",  CaseSmart, true, true);
  check_agreement("utf8 greek no-match","ελληνικά", "ζζ",  CaseSmart, true, false);
  check_agreement("utf8 cyrillic",      "привет",   "при", CaseSmart, true, true);
  check_agreement("utf8 cjk",           "文件名",    "文件", CaseSmart, true, true);
  /* Smart-case, all-lowercase query folds against an uppercase accented
     candidate (É -> é via utf8proc single-codepoint tolower). */
  check_agreement("utf8 case-fold",     "CAFÉ",     "café", CaseSmart, true, true);
  /* U+212A KELVIN SIGN lowercases from a three-byte UTF-8 sequence to the
     one-byte ASCII letter k.  This guards both matching semantics and the
     transformed pattern length under ASan/UBSan. */
  check_agreement("utf8 shrinking case-fold", "k", "K", CaseIgnore, true, true);
  check_agreement("utf8 shrinking candidate-fold", "K", "k", CaseIgnore, true, true);
  /* U+023A LATIN CAPITAL LETTER A WITH STROKE lowercases to U+2C65.
     The candidate encoding is two bytes and the folded pattern encoding is
     three, so byte-count feasibility guards incorrectly reject a match. */
  check_agreement("utf8 expanding exact fold", "Ⱥ", "'ⱥ", CaseIgnore, true, true);
  check_agreement("utf8 expanding prefix fold", "Ⱥtail", "^ⱥ", CaseIgnore, true, true);
  check_agreement("utf8 expanding suffix fold", "headȺ", "ⱥ$", CaseIgnore, true, true);
  check_agreement("utf8 expanding equal fold", "Ⱥ", "^ⱥ$", CaseIgnore, true, true);
  check_agreement("utf8 expanding fold control", "A", "'ⱥ", CaseIgnore, true, false);
  check_agreement("utf8 exact",         "héllo wörld", "'wör", CaseIgnore, true, true);
  check_agreement("utf8 suffix trims whitespace", "你 \t", "你$",
                  CaseRespect, true, true);
  check_agreement("utf8 equal rejects all-whitespace candidate", " \t", "^你$",
                  CaseRespect, true, false);
  /* Inverted non-ASCII term: must EXCLUDE candidates containing it, and KEEP
     those that don't (the false-positive direction of the deferral bug). */
  check_agreement("utf8 inverted excludes", "αβγ", "!α", CaseIgnore, true, false);
  check_agreement("utf8 inverted keeps",    "xyz", "!α", CaseIgnore, true, true);
}

static void check_bounded_range(const char *text, size_t text_len,
                                const char *pattern_text,
                                bool expect_match,
                                bool compare_legacy) {
  char *query = strdup(pattern_text);
  fzf_pattern_t *pattern = fzf_parse_pattern(
      CaseIgnore, false, query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  CHECK(pattern != NULL);
  CHECK(slab != NULL);
  if (pattern && slab) {
    bool input_is_ascii = is_ascii_utf8proc(text, text_len);
    int32_t bounded_score =
        fzf_get_score_bytes(text, text_len, pattern, slab);
    bool bounded_match =
        fzf_has_match_bytes(text, text_len, pattern, slab);
    CHECK((bounded_score > 0) == expect_match);
    CHECK(bounded_match == expect_match);
    CHECK(fzf_get_score_bytes_preclassified(
              text, text_len, input_is_ascii, pattern, slab) == bounded_score);
    CHECK(fzf_has_match_bytes_preclassified(
              text, text_len, input_is_ascii, pattern, slab) == bounded_match);
    if (compare_legacy) {
      CHECK(fzf_get_score(text, pattern, slab) == bounded_score);
      CHECK(fzf_has_match(text, pattern, slab) == bounded_match);
    }
  }
  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
  free(query);
}

static void check_bounded_entry_points(const char *text,
                                       const char *pattern_text) {
  check_bounded_range(text, strlen(text), pattern_text, true, true);
}

static void test_bounded_entry_points_derive_unicode_classification(void) {
  /* These folds cross the ASCII boundary or change UTF-8 byte length.  The
     public bounded APIs must classify the candidate rather than trust a
     caller-supplied flag. */
  check_bounded_entry_points("Kelvin", "k");
  check_bounded_entry_points("Ⱥtail", "^ⱥ");
  check_bounded_entry_points("路径/组件-123", "组件");
}

static void test_bounded_entry_points_preserve_embedded_nul(void) {
  const char text[] = {'a', '\0', 'b'};
  check_bounded_range(text, sizeof text, "b", true, false);

  char query[] = "b";
  fzf_pattern_t *pattern = fzf_parse_pattern(
      CaseIgnore, false, query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  CHECK(pattern != NULL);
  CHECK(slab != NULL);
  if (pattern && slab) {
    CHECK(fzf_get_score(text, pattern, slab) == 0);
    CHECK(!fzf_has_match(text, pattern, slab));
  }
  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
}

static void test_bounded_entry_points_need_no_terminator(void) {
  char *text = malloc(3);
  CHECK(text != NULL);
  if (!text)
    return;
  memcpy(text, "abc", 3);
  /* ASan poisons the byte after this exact allocation.  A strlen-based
     implementation therefore fails before it can inspect unrelated memory. */
  check_bounded_range(text, 3, "c", true, false);
  free(text);
}

static void test_invalid_utf8_exact_is_lossless(void) {
  /* Invalid bytes are individual lossy-decoder units.  An exact match must
     consume each unit; it must not declare success after only the valid
     prefix of the pattern. */
  check_agreement("raw exact self", "caf\xe9", "'caf\xe9",
                  CaseRespect, true, true);
  check_agreement("raw exact rejects inserted byte", "cafX\xe9", "'caf\xe9",
                  CaseRespect, true, false);
}

static void test_invalid_utf8_fuzzy_fallback_returns_positions(void) {
  char query[] = "\xe9";
  fzf_pattern_t *pattern =
      fzf_parse_pattern(CaseRespect, false, query, true);
  fzf_slab_t *slab = fzf_make_slab((fzf_slab_config_t){1, 1});
  fzf_position_t *positions = fzf_get_positions("E\xe9", pattern, slab);
  CHECK(positions != NULL);
  if (positions) {
    CHECK(positions->size == 1);
    if (positions->size == 1)
      CHECK(positions->data[0] == 1);
  }
  fzf_free_positions(positions);
  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
}

static void test_slab_allocation_failure_is_reported(void) {
  fzf_slab_t *slab =
      fzf_make_slab((fzf_slab_config_t){SIZE_MAX, SIZE_MAX});
  CHECK(slab == NULL);
  fzf_free_slab(slab);
}

static void test_utf8_char_map_scratch_reuse_and_cap(void) {
  utf8_char_map_scratch_t scratch = {0};
  const char small[] = "a\xE4\xBD\xA0z";
  utf8_char_map_t *first = utf8_build_char_map(
      small, sizeof small - 1, &scratch);
  CHECK(first == &scratch.map);
  if (!first) return;
  CHECK(!first->owned);
  CHECK(first->char_count == 3);
  CHECK(utf8_byte_to_char(first, 0) == 0);
  CHECK(utf8_byte_to_char(first, 1) == 1);
  CHECK(utf8_byte_to_char(first, 2) == 1);
  CHECK(utf8_byte_to_char(first, 4) == 2);
  size_t *retained = first->byte_to_char;
  size_t retained_capacity = scratch.byte_slot_capacity;

  utf8_char_map_t *second = utf8_build_char_map("xy", 2, &scratch);
  CHECK(second == first);
  if (!second) {
    free(scratch.map.byte_to_char);
    return;
  }
  CHECK(second->byte_to_char == retained);
  CHECK(scratch.byte_slot_capacity == retained_capacity);
  CHECK(second->char_count == 2);

  const size_t retained_slots =
      FZF_UTF8_CHAR_MAP_RETAINED_BYTES_MAX / sizeof(size_t);
  CHECK(retained_slots > 1);
  const size_t large_len = retained_slots;
  char *large = malloc(large_len);
  CHECK(large != NULL);
  if (large) {
    memset(large, 'x', large_len);
    utf8_char_map_t *one_shot = utf8_build_char_map(
        large, large_len, &scratch);
    CHECK(one_shot != NULL);
    if (one_shot) {
      CHECK(one_shot != &scratch.map);
      CHECK(one_shot->owned);
      CHECK(one_shot->char_count == large_len);
      CHECK(one_shot->byte_to_char[large_len] == large_len);
      utf8_free_char_map(one_shot);
    }
    free(large);
  }
  CHECK(scratch.map.byte_to_char == retained);
  CHECK(scratch.byte_slot_capacity == retained_capacity);
  free(scratch.map.byte_to_char);

  /* Parsed patterns fuse immutable decoded codepoints with the term object.
     Two real scorer calls reuse one slab-owned map; the slab destructor owns
     that retained allocation and sanitizer builds check the final free. */
  char query[] = "组件";
  fzf_pattern_t *pattern = fzf_parse_pattern(
      CaseSmart, false, query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  CHECK(pattern != NULL);
  CHECK(slab != NULL);
  if (pattern && slab) {
    fzf_string_t *parsed = pattern->ptr[0]->ptr[0].text;
    CHECK(parsed->codepoint_count == 2);
    CHECK(parsed->codepoints ==
          (const utf8proc_int32_t *)(parsed + 1));
    CHECK(parsed->codepoints_case_folded);
    CHECK(fzf_get_score("路径/组件-123", pattern, slab) > 0);
    size_t *scorer_retained = slab->UTF8.map.byte_to_char;
    size_t scorer_capacity = slab->UTF8.byte_slot_capacity;
    CHECK(scorer_retained != NULL);
    CHECK(scorer_capacity > 0);
    CHECK(fzf_get_score("组件", pattern, slab) > 0);
    CHECK(slab->UTF8.map.byte_to_char == scorer_retained);
    CHECK(slab->UTF8.byte_slot_capacity == scorer_capacity);
  }
  fzf_free_pattern(pattern);
  fzf_free_slab(slab);
}

static int32_t fuzzy_score_with_slab(const char *candidate, fzf_slab_t *slab) {
  fzf_string_t text = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t pattern = {.data = "fzf", .size = 3};
  fzf_position_t *positions = fzf_pos_array(0);
  CHECK(positions != NULL);
  if (!positions) {
    fzf_free_positions(positions);
    return -1;
  }
  fzf_result_t result = fzf_fuzzy_match_v2(
      true, false, &text, &pattern, positions, slab);
  fzf_free_positions(positions);
  return result.score;
}

static void test_default_score_distinguishes_boundaries(void) {
  fzf_slab_t *slab = fzf_make_default_slab();
  CHECK(slab != NULL);
  if (!slab) return;
  /* Pinned fzf gives a larger boundary bonus to whitespace and a distinct
     bonus to delimiters. */
  CHECK(fuzzy_score_with_slab("src/fzf", slab) == 84);
  CHECK(fuzzy_score_with_slab(":fzf", slab) == 84);
  CHECK(fuzzy_score_with_slab(" fzf", slab) == 88);
  CHECK(fuzzy_score_with_slab("_fzf", slab) == 80);
  fzf_free_slab(slab);
}

static void test_score_schemes_are_slab_local(void) {
  fzf_slab_t *default_slab = fzf_make_default_slab();
  fzf_slab_t *path_slab = fzf_make_default_slab();
  fzf_slab_t *history_slab = fzf_make_default_slab();
  CHECK(default_slab != NULL);
  CHECK(path_slab != NULL);
  CHECK(history_slab != NULL);
  if (!default_slab || !path_slab || !history_slab) goto done;

  CHECK(fzf_slab_set_score_scheme(path_slab, FZF_SCORE_SCHEME_PATH));
  CHECK(fzf_slab_set_score_scheme(history_slab, FZF_SCORE_SCHEME_HISTORY));
  CHECK(!fzf_slab_set_score_scheme(NULL, FZF_SCORE_SCHEME_DEFAULT));
  CHECK(!fzf_slab_set_score_scheme(default_slab,
                                   (fzf_score_scheme_t)99));

  CHECK(fuzzy_score_with_slab("src/fzf", default_slab) == 84);
  CHECK(fuzzy_score_with_slab("src/fzf", path_slab) == 84);
  CHECK(fuzzy_score_with_slab("src/fzf", history_slab) == 80);
  CHECK(fuzzy_score_with_slab(":fzf", default_slab) == 84);
  CHECK(fuzzy_score_with_slab(":fzf", path_slab) == 80);
  CHECK(fuzzy_score_with_slab(":fzf", history_slab) == 80);
  CHECK(fuzzy_score_with_slab(" fzf", default_slab) == 88);
  CHECK(fuzzy_score_with_slab(" fzf", path_slab) == 80);
  CHECK(fuzzy_score_with_slab(" fzf", history_slab) == 80);

done:
  fzf_free_slab(default_slab);
  fzf_free_slab(path_slab);
  fzf_free_slab(history_slab);
}

static void test_utf8_empty_suffix_trims_trailing_whitespace(void) {
  const char *candidate = "σa你 " "\xe2\x80\x83"; /* U+2003 EM SPACE */
  fzf_string_t text = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t pattern = {.data = "", .size = 0};
  fzf_slab_t *slab = fzf_make_default_slab();
  fzf_position_t *positions = fzf_pos_array(0);
  CHECK(slab != NULL);
  CHECK(positions != NULL);
  if (!slab || !positions) goto done;

  fzf_result_t result =
      fzf_suffix_match_utf8(true, false, &text, &pattern, positions, slab);
  CHECK(result.start == 3);
  CHECK(result.end == 3);
  CHECK(result.score == 0);
  CHECK(positions->size == 0);

done:
  fzf_free_positions(positions);
  fzf_free_slab(slab);
}

static void check_normalized_algorithm(fzf_algo_t algorithm,
                                       const char *candidate,
                                       const char *query) {
  fzf_string_t text = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t pattern = {.data = query, .size = strlen(query)};
  fzf_position_t *positions = fzf_pos_array(0);
  fzf_slab_t *slab = fzf_make_default_slab();
  CHECK(positions != NULL);
  CHECK(slab != NULL);
  if (positions && slab) {
    fzf_result_t plain = algorithm(
        true, false, &text, &pattern, positions, slab);
    positions->size = 0;
    fzf_result_t normalized = algorithm(
        true, true, &text, &pattern, positions, slab);
    CHECK(plain.start < 0);
    CHECK(normalized.start >= 0);
    CHECK(normalized.end > normalized.start);
  }
  fzf_free_slab(slab);
  fzf_free_positions(positions);
}

static void test_pinned_fzf_latin_normalization(void) {
  check_normalized_algorithm(fzf_fuzzy_match_v1_utf8, "café", "cafe");
  check_normalized_algorithm(fzf_fuzzy_match_v2_utf8, "café", "cafe");
  check_normalized_algorithm(fzf_exact_match_utf8, "café", "cafe");
  check_normalized_algorithm(fzf_prefix_match_utf8, "éclair", "ecl");
  check_normalized_algorithm(fzf_suffix_match_utf8, "cafÉ", "cafE");
  check_normalized_algorithm(fzf_equal_match_utf8, "ＦＺＦ", "FZF");
  check_normalized_algorithm(fzf_exact_match_utf8, "ɐ", "a");
  check_normalized_algorithm(fzf_exact_match_utf8, "Ấ", "A");
  check_normalized_algorithm(fzf_exact_match_utf8, "Ờ", "O");
  check_normalized_algorithm(fzf_exact_match_utf8, "ự", "u");

  char normalized_query[] = "cafe";
  fzf_pattern_t *normalized = fzf_parse_pattern(
      CaseRespect, true, normalized_query, true);
  char plain_query[] = "cafe";
  fzf_pattern_t *plain = fzf_parse_pattern(
      CaseRespect, false, plain_query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  CHECK(normalized != NULL);
  CHECK(plain != NULL);
  CHECK(slab != NULL);
  if (normalized && plain && slab) {
    CHECK(normalized->ptr[0]->ptr[0].normalize);
    CHECK(!plain->ptr[0]->ptr[0].normalize);
    CHECK(fzf_get_score("café", normalized, slab) > 0);
    CHECK(fzf_has_match("café", normalized, slab));
    CHECK(fzf_get_score("café", plain, slab) == 0);
    CHECK(!fzf_has_match("café", plain, slab));
  }
  fzf_free_slab(slab);
  fzf_free_pattern(plain);
  fzf_free_pattern(normalized);
}

int main(void) {
  printf("--- fzf-additions: fzf_has_match ---\n");
  RUN(test_fuzzy_basic_match);
  RUN(test_fuzzy_basic_no_match);
  RUN(test_fuzzy_empty_pattern);
  RUN(test_fuzzy_pattern_longer_than_text);
  RUN(test_exact_match);
  RUN(test_exact_no_match);
  RUN(test_prefix_match);
  RUN(test_prefix_no_match);
  RUN(test_suffix_match);
  RUN(test_suffix_no_match);
  RUN(test_anchored_matches_trim_candidate_whitespace);
  RUN(test_equal_match);
  RUN(test_equal_no_match_different_string);
  RUN(test_equal_preserves_pattern_edge_whitespace);
  RUN(test_parsed_equal_preserves_escaped_edge_whitespace);
  RUN(test_negation_term_excludes);
  RUN(test_and_across_term_sets);
  RUN(test_or_within_term_set);
  RUN(test_or_satisfied_only_by_inverse_term);
  RUN(test_small_slab_long_gap_preserves_match);
  RUN(test_small_slab_inverse_long_gap_preserves_membership);
  RUN(test_utf8_v1_reverse_scan_tightens_match);
  RUN(test_case_ignore);
  RUN(test_case_respect_matches_when_case_aligns);
  RUN(test_case_respect_no_match_when_case_differs);
  RUN(test_smart_case_lowercase_query_ignores_case);
  RUN(test_smart_case_uppercase_query_respects_case);
  RUN(test_compound_pattern);
  RUN(test_utf8_terms);
  RUN(test_bounded_entry_points_derive_unicode_classification);
  RUN(test_bounded_entry_points_preserve_embedded_nul);
  RUN(test_bounded_entry_points_need_no_terminator);
  RUN(test_invalid_utf8_exact_is_lossless);
  RUN(test_invalid_utf8_fuzzy_fallback_returns_positions);
  RUN(test_slab_allocation_failure_is_reported);
  RUN(test_utf8_char_map_scratch_reuse_and_cap);
  RUN(test_default_score_distinguishes_boundaries);
  RUN(test_score_schemes_are_slab_local);
  RUN(test_utf8_empty_suffix_trims_trailing_whitespace);
  RUN(test_pinned_fzf_latin_normalization);

  if (failed == 0) {
    printf("\nAll fzf-additions tests passed.\n");
    return 0;
  } else {
    printf("\n%d check(s) failed.\n", failed);
    return 1;
  }
}
