/* SPDX-License-Identifier: MIT */
/* Shared scorer tail for the exported score entry points.

   The including function must define input, input_is_ascii, pattern, and slab.
   This statement include keeps each exported entry point in its own hot frame.
   It avoids compiler-specific inline attributes and preserves legacy scorer
   performance when link-time optimization is unavailable. */
if (pattern->only_inv) {
  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *term_set = pattern->ptr[i];
    fzf_term_t *term = &term_set->ptr[0];
    /* Negation is a membership decision, not a ranking decision.  The v1
       fallback can return a valid match with a non-positive raw score when
       the matching characters have a long gap.  Testing the score here made a
       small slab accept candidates that the default slab rejected. */
    fzf_result_t res = CALL_ALG(
        term, false, input, input_is_ascii, NULL, slab);
    if (fzf_allocation_failed()) return 0;
    if (res.start >= 0) return 0;
  }
  return 1;
}

int32_t total_score = 0;
for (size_t i = 0; i < pattern->size; i++) {
  fzf_term_set_t *term_set = pattern->ptr[i];
  int32_t current_score = 0;
  bool matched = false;
  for (size_t j = 0; j < term_set->size; j++) {
    fzf_term_t *term = &term_set->ptr[j];
    fzf_result_t res = CALL_ALG(
        term, false, input, input_is_ascii, NULL, slab);
    if (fzf_allocation_failed()) return 0;
    if (res.start >= 0) {
      if (term->inv) continue;
      /* Score zero is the public no-match sentinel.  Some valid v1 matches
         with long gaps can have a non-positive raw score.  This can happen
         when v2 falls back because a slab is small.  Preserve membership and
         use the lowest rankable score. */
      current_score = res.score > 0 ? res.score : 1;
      matched = true;
      break;
    }

    if (term->inv) {
      /* A set can be satisfied only by an inverse term, as in `!foo | bar'.
         Keep inverse terms score-neutral so `foo !bar' keeps foo's score.
         The final return converts an all-neutral match to the minimum score. */
      current_score = 0;
      matched = true;
    }
  }
  if (!matched) return 0;
  total_score += current_score;
}

return total_score > 0 ? total_score : 1;
