/* SPDX-License-Identifier: GPL-3.0-or-later
 * fzf-additions.c — extensions to the upstream fzf scorer that aren't in
 * fzf.h, kept here so future syncs with upstream don't conflict.
 *
 *   fzf_has_match(text, pattern) -> bool
 *
 *     Returns true iff PATTERN matches TEXT, with no score computation.
 *     fzf's `fzf_get_score' returns 0 when the pattern doesn't match and a
 *     positive integer otherwise — so the caller can already use it as a
 *     boolean — but the scoring computation itself is O(text_len × pattern_len)
 *     for fuzzy-v2 (the default algorithm) plus a slab allocation per call.
 *     When the caller only needs the match decision (e.g. to build a filter
 *     set for refinement), fzf_has_match skips the DP table entirely:
 *
 *       fuzzy_v1 / fuzzy_v2  O(text_len + pattern_len)
 *       exact_match          O(text_len × pattern_len) worst case, O(n) avg
 *       prefix / suffix      O(pattern_len)
 *       equal                O(pattern_len)
 *
 *     The match decision is identical to fzf_get_score's; only the score
 *     and the position vector are skipped.  Pattern composition (AND across
 *     term-sets, OR within a term-set, `inv' negation) mirrors fzf's logic.
 */

#include "fzf.h"

#include <ctype.h>
#include <string.h>

static inline char fzf_addn_lower(unsigned char c) {
  return (char)tolower(c);
}

/* Fuzzy: the pattern's characters appear in TEXT in order.  Pattern is
   already case-normalized by fzf_parse_pattern when case_sensitive is
   false, so we only need to lowercase the text side. */
static bool fzf_addn_fuzzy(bool case_sensitive,
                           const char *text, size_t tn,
                           const char *pat,  size_t pn) {
  if (pn == 0) return true;
  if (tn < pn) return false;
  size_t pi = 0;
  if (case_sensitive) {
    for (size_t ti = 0; ti < tn && pi < pn; ti++)
      if (text[ti] == pat[pi]) pi++;
  } else {
    for (size_t ti = 0; ti < tn && pi < pn; ti++)
      if (fzf_addn_lower((unsigned char)text[ti]) == pat[pi]) pi++;
  }
  return pi == pn;
}

/* Exact: PATTERN appears as a contiguous substring of TEXT.  No memmem
   — not portable — so use a hand-rolled scan. */
static bool fzf_addn_exact(bool case_sensitive,
                           const char *text, size_t tn,
                           const char *pat,  size_t pn) {
  if (pn == 0) return true;
  if (tn < pn) return false;
  for (size_t i = 0; i + pn <= tn; i++) {
    bool eq = true;
    if (case_sensitive) {
      eq = (memcmp(text + i, pat, pn) == 0);
    } else {
      for (size_t j = 0; j < pn; j++)
        if (fzf_addn_lower((unsigned char)text[i + j]) != pat[j]) {
          eq = false; break;
        }
    }
    if (eq) return true;
  }
  return false;
}

static bool fzf_addn_prefix(bool case_sensitive,
                            const char *text, size_t tn,
                            const char *pat,  size_t pn) {
  if (pn == 0) return true;
  if (tn < pn) return false;
  if (case_sensitive) return memcmp(text, pat, pn) == 0;
  for (size_t i = 0; i < pn; i++)
    if (fzf_addn_lower((unsigned char)text[i]) != pat[i]) return false;
  return true;
}

static bool fzf_addn_suffix(bool case_sensitive,
                            const char *text, size_t tn,
                            const char *pat,  size_t pn) {
  if (pn == 0) return true;
  if (tn < pn) return false;
  const char *tail = text + (tn - pn);
  if (case_sensitive) return memcmp(tail, pat, pn) == 0;
  for (size_t i = 0; i < pn; i++)
    if (fzf_addn_lower((unsigned char)tail[i]) != pat[i]) return false;
  return true;
}

static bool fzf_addn_equal(bool case_sensitive,
                           const char *text, size_t tn,
                           const char *pat,  size_t pn) {
  if (tn != pn) return false;
  if (case_sensitive) return memcmp(text, pat, pn) == 0;
  for (size_t i = 0; i < pn; i++)
    if (fzf_addn_lower((unsigned char)text[i]) != pat[i]) return false;
  return true;
}

/* Dispatch one term to the right cheap match implementation.  Uses
   function-pointer identity to identify the algorithm — same trick the
   upstream fzf evaluator uses, so dispatch stays in lockstep without
   shipping a parallel enum.

   IMPORTANT: the matched-against pattern lives in `term->text`
   (a `fzf_string_t *`), not `term->ptr`.  `ptr` retains the original
   input including prefix tokens (`!`, `'`, `^`, trailing `$`); after
   fzf_parse_pattern strips those, only `text->data`/`text->size` reflect
   the actual bytes to compare against. */
static bool fzf_addn_term(const fzf_term_t *term,
                          const char *text, size_t tn) {
  fzf_string_t *pat = (fzf_string_t *)term->text;
  const char *p     = pat ? pat->data : "";
  size_t      pn    = pat ? pat->size : 0;
  bool match;
  if (term->fn == fzf_fuzzy_match_v1 || term->fn == fzf_fuzzy_match_v2)
    match = fzf_addn_fuzzy (term->case_sensitive, text, tn, p, pn);
  else if (term->fn == fzf_exact_match_naive)
    match = fzf_addn_exact (term->case_sensitive, text, tn, p, pn);
  else if (term->fn == fzf_prefix_match)
    match = fzf_addn_prefix(term->case_sensitive, text, tn, p, pn);
  else if (term->fn == fzf_suffix_match)
    match = fzf_addn_suffix(term->case_sensitive, text, tn, p, pn);
  else if (term->fn == fzf_equal_match)
    match = fzf_addn_equal (term->case_sensitive, text, tn, p, pn);
  else
    /* Unknown algorithm — fall back to "no match" rather than guess. */
    match = false;
  return term->inv ? !match : match;
}

/* True when FN is one of the ASCII-only algorithm variants that
   fzf_addn_term evaluates cheaply.  fzf_parse_pattern assigns the `_utf8'
   variants for any term containing non-ASCII bytes; those (and any future
   algorithm) are not byte-wise matchable here and must go through the full
   scorer instead. */
static bool fzf_addn_is_ascii_algo(fzf_algo_t fn) {
  return fn == fzf_fuzzy_match_v1 || fn == fzf_fuzzy_match_v2 ||
         fn == fzf_exact_match_naive || fn == fzf_prefix_match ||
         fn == fzf_suffix_match || fn == fzf_equal_match;
}

bool fzf_has_match(const char *text, fzf_pattern_t *pattern, fzf_slab_t *slab) {
  if (!pattern || pattern->size == 0) return true;
  /* The cheap matchers below are byte-wise ASCII only (no Unicode case
     folding) and dispatch on the ASCII algorithm function pointers.  If any
     term uses a `_utf8' variant (chosen by fzf_parse_pattern for non-ASCII
     query terms) or any unrecognized algorithm, defer to the full scorer,
     whose match decision is authoritative — including correct handling of
     Unicode case folding and `inv' negation. */
  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *set = pattern->ptr[i];
    for (size_t j = 0; j < set->size; j++)
      if (!fzf_addn_is_ascii_algo(set->ptr[j].fn))
        return fzf_get_score(text, pattern, slab) > 0;
  }
  size_t tn = strlen(text);
  /* AND across term-sets, OR within each term-set — same composition as
     fzf_get_score, just collapsed to bool. */
  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *set = pattern->ptr[i];
    bool any = false;
    for (size_t j = 0; j < set->size; j++) {
      if (fzf_addn_term(&set->ptr[j], text, tn)) { any = true; break; }
    }
    if (!any) return false;
  }
  return true;
}
