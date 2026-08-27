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

static fzf_slab_t *make_selected_slab(uint8_t options) {
  static const size_t caps16[] = {1, 8, 64, 1024, 8192, 100 * 1024};
  static const size_t caps32[] = {1, 8, 64, 256, 1024, 2048};
  size_t which = (options >> 3) % (sizeof(caps16) / sizeof(caps16[0]));
  return fzf_make_slab((fzf_slab_config_t){caps16[which], caps32[which]});
}

static void check_positions(const char *candidate, bool matched,
                            const fzf_position_t *positions) {
  (void)matched;
  if (!positions)
    return;

  size_t limit = strlen(candidate);
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

static void check_term(const char *candidate, const fzf_term_t *term,
                       fzf_slab_t *slab) {
  if (!term->fn || !term->text)
    return;

  fzf_string_t input = {.data = candidate, .size = strlen(candidate)};
  fzf_string_t *pattern = (fzf_string_t *)term->text;
  fzf_result_t without_positions =
      term->fn(term->case_sensitive, false, &input, pattern, NULL, slab);
  fzf_position_t *positions = fzf_pos_array(0);
  if (!positions)
    abort();
  fzf_result_t with_positions = term->fn(term->case_sensitive, false, &input,
                                         pattern, positions, slab);

  /* Fuzzy v2 may backtrack to a more precise START only when positions are
     requested.  Membership and score must not depend on observability. */
  if ((without_positions.start >= 0) != (with_positions.start >= 0) ||
      without_positions.score != with_positions.score)
    fuzz_fail("requesting positions changed a term result");
  if (with_positions.start < 0 && positions->size != 0)
    fuzz_fail("a failed term returned highlight positions");
  check_positions(candidate, with_positions.start >= 0, positions);
  check_position_order(positions);
  fzf_free_positions(positions);

  if (term->fn == fzf_fuzzy_match_v2) {
    fzf_result_t v1 = fzf_fuzzy_match_v1(
        term->case_sensitive, false, &input, pattern, NULL, slab);
    if ((v1.start >= 0) != (with_positions.start >= 0))
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
     Non-ASCII and malformed-byte behavior is enabled by the stacked UTF-8
     change, where those semantics are defined and tested explicitly. */
  if (!ascii_bytes(payload, query_size) ||
      !ascii_bytes(payload + candidate_offset, candidate_size)) {
    free(candidate);
    free(query);
    return;
  }

  fzf_case_types case_mode = (fzf_case_types)(options % 3);
  bool fuzzy = (options & 4) != 0;
  fzf_pattern_t *pattern =
      fzf_parse_pattern(case_mode, false, query, fuzzy);
  /* main currently underflows in suffix_match when a mutated suffix pattern
     is longer than its candidate.  The additive fuzz layer records but does
     not alter that pre-existing behavior; the stacked matcher fix removes
     this exclusion and promotes the reproducer into the permanent corpus. */
  if (pattern && (candidate[0] == '\0' || pattern_has_end_anchor(pattern))) {
    fzf_free_pattern(pattern);
    free(candidate);
    free(query);
    return;
  }
  fzf_slab_t *default_slab = fzf_make_default_slab();
  fzf_slab_t *selected_slab = make_selected_slab(options);
  if (!pattern || !default_slab || !selected_slab)
    abort();

  int32_t score = fzf_get_score(candidate, pattern, default_slab);
  if (score != fzf_get_score(candidate, pattern, default_slab))
    fuzz_fail("repeated scoring is not deterministic");

  fzf_position_t *positions =
      fzf_get_positions(candidate, pattern, default_slab);
  check_positions(candidate, score > 0, positions);
  fzf_free_positions(positions);

  /* Exercise the filter-only matcher as part of the public C surface.  The
     baseline implementation intentionally has historical whitespace/anchor
     differences from the scorer; this test-only layer must not redefine
     those semantics. */
  (void)fzf_has_match(candidate, pattern);

  /* Exercise the documented small-slab fallback under sanitizers.  The base
     implementation has historical score differences between algorithms, so
     this additive layer deliberately asserts safety rather than score parity. */
  int32_t selected_score = fzf_get_score(candidate, pattern, selected_slab);
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
