/* SPDX-License-Identifier: GPL-3.0-or-later */
#ifndef FZF_ADDITIONS_H_
#define FZF_ADDITIONS_H_

/* This matcher generation gives fzf_has_match a slab for its Unicode
   fallback.  Additive callers can retain compatibility with the two-argument
   baseline by selecting the signature through this feature macro. */
#define FZF_NATIVE_HAS_MATCH_SLAB 1

#include "fzf.h"

/* See fzf-additions.c.  SLAB is used only for the full-scorer fallback taken
   when PATTERN contains a non-ASCII (UTF-8) or otherwise unrecognized term;
   pure-ASCII patterns never touch it. */
bool fzf_has_match(const char *text, fzf_pattern_t *pattern, fzf_slab_t *slab);
/* Safe counterpart for a bounded byte string.  TEXT must reference at least
   TEXT_LEN readable bytes.  Every byte in that range is candidate data,
   including embedded NUL bytes.  The legacy fzf_has_match wrapper stops at
   the first NUL.  This function derives the exact range's ASCII
   classification before it selects a matcher. */
bool fzf_has_match_bytes(const char *text, size_t text_len,
                         fzf_pattern_t *pattern, fzf_slab_t *slab);

#endif /* FZF_ADDITIONS_H_ */
