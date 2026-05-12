/* Standalone C unit tests for fzf-additions.c (fzf_has_match).
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
  bool    addn_says  = fzf_has_match(text, p);

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
  RUN(test_equal_match);
  RUN(test_equal_no_match_different_string);
  RUN(test_negation_term_excludes);
  RUN(test_and_across_term_sets);
  RUN(test_or_within_term_set);
  RUN(test_case_ignore);
  RUN(test_case_respect_matches_when_case_aligns);
  RUN(test_case_respect_no_match_when_case_differs);
  RUN(test_smart_case_lowercase_query_ignores_case);
  RUN(test_smart_case_uppercase_query_respects_case);
  RUN(test_compound_pattern);

  if (failed == 0) {
    printf("\nAll fzf-additions tests passed.\n");
    return 0;
  } else {
    printf("\n%d check(s) failed.\n", failed);
    return 1;
  }
}
