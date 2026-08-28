// SPDX-License-Identifier: MIT
#ifndef FZF_H_
#define FZF_H_

/* Feature contract for additive tooling such as the standalone fuzz layer.
   Callers may use this to admit arbitrary byte strings only when the matcher
   defines UTF-8 and malformed-byte behavior. */
#define FZF_NATIVE_UTF8_MATCHING 1

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include "utf8_char_index.h"

typedef struct {
  int16_t *data;
  size_t size;
  size_t cap;
  bool allocated;
} fzf_i16_t;

typedef struct {
  int32_t *data;
  size_t size;
  size_t cap;
  bool allocated;
} fzf_i32_t;

typedef struct {
  uint32_t *data;
  size_t size;
  size_t cap;
} fzf_position_t;

typedef struct {
  int32_t start;
  int32_t end;
  int32_t score;
} fzf_result_t;

typedef struct {
  fzf_i16_t I16;
  fzf_i32_t I32;
  /* Thread-confined high-water scratch.  As with I16/I32, a slab must not be
     used by overlapping scoring calls; fzf_free_slab owns its allocation. */
  utf8_char_map_scratch_t UTF8;
} fzf_slab_t;

typedef struct {
  size_t size_16;
  size_t size_32;
} fzf_slab_config_t;

typedef struct {
  const char *data;
  size_t size;
  /* Parsed patterns own an immutable decoded representation in the same
     allocation as this struct.  It can be shared by scoring threads and is
     released with the struct.  Ordinary caller-provided strings leave these
     fields zero and take the compatibility decode path. */
  const utf8proc_int32_t *codepoints;
  size_t codepoint_count;
  bool codepoints_case_folded;
} fzf_string_t;

typedef fzf_result_t (*fzf_algo_t)(bool, bool, fzf_string_t *, fzf_string_t *,
                                   fzf_position_t *, fzf_slab_t *);

typedef enum { CaseSmart = 0, CaseIgnore, CaseRespect } fzf_case_types;

typedef struct {
  fzf_algo_t fn;
  bool inv;
  char *ptr;
  void *text;
  bool case_sensitive;
} fzf_term_t;

typedef struct {
  fzf_term_t *ptr;
  size_t size;
  size_t cap;
} fzf_term_set_t;

typedef struct {
  fzf_term_set_t **ptr;
  size_t size;
  size_t cap;
  bool only_inv;
} fzf_pattern_t;

/* Scoring APIs use ordinary match/no-match return values, so allocation
   failure is reported separately.  The flag is thread-local: callers must
   inspect it immediately after fzf_get_score/fzf_get_positions (or an
   algorithm call) on the same thread. */
void fzf_clear_allocation_failure(void);
bool fzf_allocation_failed(void);

fzf_result_t fzf_fuzzy_match_v1(bool case_sensitive, bool normalize,
                                fzf_string_t *text, fzf_string_t *pattern,
                                fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_fuzzy_match_v2(bool case_sensitive, bool normalize,
                                fzf_string_t *text, fzf_string_t *pattern,
                                fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_exact_match_naive(bool case_sensitive, bool normalize,
                                   fzf_string_t *text, fzf_string_t *pattern,
                                   fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_prefix_match(bool case_sensitive, bool normalize,
                              fzf_string_t *text, fzf_string_t *pattern,
                              fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_suffix_match(bool case_sensitive, bool normalize,
                              fzf_string_t *text, fzf_string_t *pattern,
                              fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_equal_match(bool case_sensitive, bool normalize,
                             fzf_string_t *text, fzf_string_t *pattern,
                             fzf_position_t *pos, fzf_slab_t *slab);

/* interface */
fzf_pattern_t *fzf_parse_pattern(fzf_case_types case_mode, bool normalize,
                                 char *pattern, bool fuzzy);
void fzf_free_pattern(fzf_pattern_t *pattern);

int32_t fzf_get_score(const char *text, fzf_pattern_t *pattern,
                      fzf_slab_t *slab);
/* Safe entry point for callers that own a bounded byte string.  The function
   derives the byte range's ASCII classification.  Semantics and
   allocation-failure reporting are otherwise identical to fzf_get_score. */
int32_t fzf_get_score_bytes(const char *text, size_t text_len,
                            fzf_pattern_t *pattern, fzf_slab_t *slab);

fzf_position_t *fzf_pos_array(size_t len);
fzf_position_t *fzf_get_positions(const char *text, fzf_pattern_t *pattern,
                                  fzf_slab_t *slab);
void fzf_free_positions(fzf_position_t *pos);

fzf_slab_t *fzf_make_slab(fzf_slab_config_t config);
fzf_slab_t *fzf_make_default_slab(void);
void fzf_free_slab(fzf_slab_t *slab);

/* UTF-8 utility functions for testing */
bool is_ascii_utf8proc(const char *text, size_t len);
int32_t char_class_of_utf8proc(utf8proc_int32_t codepoint);
utf8proc_int32_t utf8proc_case_fold(utf8proc_int32_t codepoint);
int32_t utf8_fuzzy_index(fzf_string_t *input, const char *pattern,
                         size_t pattern_len, bool case_sensitive);

/* UTF-8 aware matching algorithms */
fzf_result_t fzf_exact_match_utf8(bool case_sensitive, bool normalize,
                                  fzf_string_t *text, fzf_string_t *pattern,
                                  fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_prefix_match_utf8(bool case_sensitive, bool normalize,
                                   fzf_string_t *text, fzf_string_t *pattern,
                                   fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_suffix_match_utf8(bool case_sensitive, bool normalize,
                                   fzf_string_t *text, fzf_string_t *pattern,
                                   fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_equal_match_utf8(bool case_sensitive, bool normalize,
                                  fzf_string_t *text, fzf_string_t *pattern,
                                  fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_fuzzy_match_v1_utf8(bool case_sensitive, bool normalize,
                                     fzf_string_t *text, fzf_string_t *pattern,
                                     fzf_position_t *pos, fzf_slab_t *slab);
fzf_result_t fzf_fuzzy_match_v2_utf8(bool case_sensitive, bool normalize,
                                     fzf_string_t *text, fzf_string_t *pattern,
                                     fzf_position_t *pos, fzf_slab_t *slab);

#endif // FZF_H_
