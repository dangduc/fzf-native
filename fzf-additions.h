/* SPDX-License-Identifier: GPL-3.0-or-later */
#ifndef FZF_ADDITIONS_H_
#define FZF_ADDITIONS_H_

#include "fzf.h"

/* See fzf-additions.c.  SLAB is used only for the full-scorer fallback taken
   when PATTERN contains a non-ASCII (UTF-8) or otherwise unrecognized term;
   pure-ASCII patterns never touch it. */
bool fzf_has_match(const char *text, fzf_pattern_t *pattern, fzf_slab_t *slab);

#endif /* FZF_ADDITIONS_H_ */
