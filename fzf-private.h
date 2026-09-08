/* SPDX-License-Identifier: MIT */
#ifndef FZF_PRIVATE_H_
#define FZF_PRIVATE_H_

#include "fzf.h"

#ifdef _MSC_VER
#define FZF_THREAD_LOCAL __declspec(thread)
#else
#define FZF_THREAD_LOCAL _Thread_local
#endif

/* Workers disable SIMD for cache-selected candidate subsets, where matches
   are deliberately dense and scalar search is cheaper. */
extern FZF_THREAD_LOCAL bool fzf_simd_prefilter_allowed;

static inline void fzf_set_simd_prefilter_allowed(bool allowed) {
  fzf_simd_prefilter_allowed = allowed;
}

static inline bool fzf_get_simd_prefilter_allowed(void) {
  return fzf_simd_prefilter_allowed;
}

/* Internal fast paths for callers that classified the exact byte range.
   INPUT_IS_ASCII must describe TEXT[0..TEXT_LEN).  Public callers must use the
   safe bounded functions in fzf.h and fzf-additions.h instead. */
int32_t fzf_get_score_bytes_preclassified(
    const char *text, size_t text_len, bool input_is_ascii,
    fzf_pattern_t *pattern, fzf_slab_t *slab);
bool fzf_has_match_bytes_preclassified(
    const char *text, size_t text_len, bool input_is_ascii,
    fzf_pattern_t *pattern, fzf_slab_t *slab);

#endif /* FZF_PRIVATE_H_ */
