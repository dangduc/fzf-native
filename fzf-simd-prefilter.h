// SPDX-License-Identifier: MIT
/*
 * Bounded SIMD prefilters for ASCII fuzzy and literal matching.
 *
 * The chunk-reuse fuzzy scan and two-seed literal scan are adapted from
 * Frizbee at commit b9b42b2f915a992264e917088810841df759cc1d.  Unlike
 * Frizbee's Rust implementation, every vector load here stays inside the C
 * object's readable range.  The byte-frequency ranks used to choose literal
 * seeds come from memchr's packed-pair search.  See LICENSE-MIT.
 */
#ifndef FZF_SIMD_PREFILTER_H_
#define FZF_SIMD_PREFILTER_H_

#include "fzf.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(__aarch64__) || defined(_M_ARM64)
#include <arm_neon.h>
#define FZF_HAVE_SIMD_PREFILTER 1
#define FZF_SIMD_NEON 1
#elif defined(__x86_64__) || defined(_M_X64)
#include <emmintrin.h>
#if defined(_MSC_VER)
#include <intrin.h>
#endif
#define FZF_HAVE_SIMD_PREFILTER 1
#define FZF_SIMD_SSE2 1
#else
#define FZF_HAVE_SIMD_PREFILTER 0
#endif

#define FZF_SIMD_LANES 16u
#define FZF_SIMD_FUZZY_MIN_PATTERN 5u
#define FZF_SIMD_FUZZY_MIN_TEXT 24u
#define FZF_SIMD_LITERAL_MIN_STARTS 32u
#define FZF_SIMD_MAX_PATTERN 256u
/* The plan's strictest member is size_t.  Spell the alignment this way
   instead of using C11 _Alignof so the header also builds as MSVC C. */
#define FZF_SIMD_PLAN_ALIGNMENT sizeof(size_t)

typedef struct {
  uint8_t exact[FZF_SIMD_LANES];
  uint8_t alternate[FZF_SIMD_LANES];
} fzf_simd_query_byte_t;

typedef struct {
  size_t pattern_size;
  size_t seed_a_offset;
  size_t seed_b_offset;
  /* Borrowed from the parser-owned term allocation for direct exact checks. */
  const char *pattern;
  bool case_sensitive;
  fzf_simd_query_byte_t bytes[];
} fzf_ascii_query_plan_t;

static inline size_t fzf_ascii_query_plan_size(size_t pattern_size) {
#if FZF_HAVE_SIMD_PREFILTER
  if (pattern_size >
      (SIZE_MAX - sizeof(fzf_ascii_query_plan_t)) /
          sizeof(fzf_simd_query_byte_t))
    return 0;
  return sizeof(fzf_ascii_query_plan_t) +
         pattern_size * sizeof(fzf_simd_query_byte_t);
#else
  (void)pattern_size;
  return 0;
#endif
}

/* Parsed patterns keep the public decoded-codepoint layout and place this
   private plan after that array.  Public algorithm entry points never call
   this helper for caller-owned strings. */
static inline const fzf_ascii_query_plan_t *
fzf_ascii_query_plan_from_parsed(const fzf_string_t *pattern) {
#if FZF_HAVE_SIMD_PREFILTER
  uintptr_t after_codepoints =
      (uintptr_t)(pattern->codepoints + pattern->codepoint_count);
  size_t alignment = FZF_SIMD_PLAN_ALIGNMENT;
  uintptr_t aligned =
      (after_codepoints + alignment - 1) & ~(uintptr_t)(alignment - 1);
  return (const fzf_ascii_query_plan_t *)aligned;
#else
  (void)pattern;
  return NULL;
#endif
}

static inline size_t fzf_ascii_query_plan_private_size(
    const fzf_string_t *pattern) {
#if FZF_HAVE_SIMD_PREFILTER
  const unsigned char *after_codepoints =
      (const unsigned char *)(pattern->codepoints +
                              pattern->codepoint_count);
  const fzf_ascii_query_plan_t *plan =
      fzf_ascii_query_plan_from_parsed(pattern);
  return (size_t)((const unsigned char *)plan - after_codepoints) +
         fzf_ascii_query_plan_size(plan->pattern_size);
#else
  (void)pattern;
  return 0;
#endif
}

#if FZF_HAVE_SIMD_PREFILTER

/* Lower ranks are rarer.  This is the ASCII half of memchr's default
   packed-pair rank table (Copyright 2015 Andrew Gallant, MIT). */
static const uint8_t fzf_ascii_byte_rank[128] = {
    55,  52,  51,  50,  49,  48,  47,  46,  45,  103, 242, 66,  67,
    229, 44,  43,  42,  41,  40,  39,  38,  37,  36,  35,  34,  33,
    56,  32,  31,  30,  29,  28,  255, 148, 164, 149, 136, 160, 155,
    173, 221, 222, 134, 122, 232, 202, 215, 224, 208, 220, 204, 187,
    183, 179, 177, 168, 178, 200, 226, 195, 154, 184, 174, 126, 120,
    191, 157, 194, 170, 189, 162, 161, 150, 193, 142, 137, 171, 176,
    185, 167, 186, 112, 175, 192, 188, 156, 140, 143, 123, 133, 128,
    147, 138, 146, 114, 223, 151, 249, 216, 238, 236, 253, 227, 218,
    230, 247, 135, 180, 241, 233, 246, 244, 231, 139, 245, 243, 251,
    235, 201, 196, 240, 214, 152, 182, 205, 181, 127, 27,
};

static inline uint8_t fzf_ascii_rank(uint8_t byte) {
  return byte < 128 ? fzf_ascii_byte_rank[byte] : UINT8_MAX;
}

static inline void fzf_ascii_seed_offsets(const uint8_t *pattern,
                                          size_t pattern_size,
                                          size_t *seed_a,
                                          size_t *seed_b) {
  size_t first = 0;
  for (size_t i = 1; i < pattern_size; i++)
    if (fzf_ascii_rank(pattern[i]) < fzf_ascii_rank(pattern[first])) first = i;

  /* Prefer a distinct byte value even when the rarest byte is repeated.
     Repeating one seed at two offsets is correct but needlessly weak. */
  size_t second = SIZE_MAX;
  for (size_t i = 0; i < pattern_size; i++)
    if (pattern[i] != pattern[first] &&
        (second == SIZE_MAX ||
         fzf_ascii_rank(pattern[i]) < fzf_ascii_rank(pattern[second])))
      second = i;
  if (second == SIZE_MAX) second = first == 0 ? 1 : 0;
  if (first <= second) {
    *seed_a = first;
    *seed_b = second;
  } else {
    *seed_a = second;
    *seed_b = first;
  }
}

static inline void fzf_ascii_query_plan_init(void *storage,
                                             const char *pattern,
                                             size_t pattern_size,
                                             bool case_sensitive) {
  fzf_ascii_query_plan_t *plan = storage;
  plan->pattern_size = pattern_size;
  plan->seed_a_offset = 0;
  plan->seed_b_offset = 0;
  plan->pattern = pattern;
  plan->case_sensitive = case_sensitive;
  if (pattern_size >= 2)
    fzf_ascii_seed_offsets((const uint8_t *)pattern, pattern_size,
                           &plan->seed_a_offset, &plan->seed_b_offset);

  for (size_t i = 0; i < pattern_size; i++) {
    uint8_t exact = (uint8_t)pattern[i];
    uint8_t alternate = exact;
    if (!case_sensitive && exact >= 'a' && exact <= 'z')
      alternate = (uint8_t)(exact - ('a' - 'A'));
    memset(plan->bytes[i].exact, exact, FZF_SIMD_LANES);
    memset(plan->bytes[i].alternate, alternate, FZF_SIMD_LANES);
  }
}

#if defined(FZF_SIMD_NEON)
typedef uint8x16_t fzf_simd_chunk_t;

static inline fzf_simd_chunk_t fzf_simd_load(const void *address) {
  return vld1q_u8((const uint8_t *)address);
}

static inline uint16_t fzf_simd_movemask(uint8x16_t mask) {
  static const uint8_t weights[16] = {
      1, 2, 4, 8, 16, 32, 64, 128,
      1, 2, 4, 8, 16, 32, 64, 128,
  };
  uint8x16_t bits = vandq_u8(mask, vld1q_u8(weights));
  uint16_t low = (uint16_t)vaddv_u8(vget_low_u8(bits));
  uint16_t high = (uint16_t)vaddv_u8(vget_high_u8(bits));
  return (uint16_t)(low | (uint16_t)(high << 8));
}

static inline uint16_t fzf_simd_occurrences(
    fzf_simd_chunk_t chunk, const fzf_simd_query_byte_t *query,
    bool case_sensitive) {
  uint8x16_t exact = vceqq_u8(chunk, fzf_simd_load(query->exact));
  if (case_sensitive) return fzf_simd_movemask(exact);
  return fzf_simd_movemask(
      vorrq_u8(exact, vceqq_u8(chunk, fzf_simd_load(query->alternate))));
}
#elif defined(FZF_SIMD_SSE2)
typedef __m128i fzf_simd_chunk_t;

static inline fzf_simd_chunk_t fzf_simd_load(const void *address) {
  return _mm_loadu_si128((const __m128i *)address);
}

static inline uint16_t fzf_simd_occurrences(
    fzf_simd_chunk_t chunk, const fzf_simd_query_byte_t *query,
    bool case_sensitive) {
  __m128i exact = _mm_cmpeq_epi8(chunk, fzf_simd_load(query->exact));
  if (case_sensitive) return (uint16_t)_mm_movemask_epi8(exact);
  __m128i alternate =
      _mm_cmpeq_epi8(chunk, fzf_simd_load(query->alternate));
  return (uint16_t)_mm_movemask_epi8(_mm_or_si128(exact, alternate));
}
#endif

static inline unsigned int fzf_simd_first_lane(uint16_t mask) {
#if defined(_MSC_VER)
  unsigned long lane;
  _BitScanForward(&lane, (unsigned long)mask);
  return (unsigned int)lane;
#else
  return (unsigned int)__builtin_ctz((unsigned int)mask);
#endif
}

static inline bool fzf_ascii_plan_byte_matches(
    uint8_t candidate, const fzf_simd_query_byte_t *query,
    bool case_sensitive) {
  return candidate == query->exact[0] ||
         (!case_sensitive && candidate == query->alternate[0]);
}

static inline const char *fzf_ascii_plan_find_byte(
    const char *text, size_t text_size,
    const fzf_simd_query_byte_t *query, bool case_sensitive) {
  const char *exact = memchr(text, query->exact[0], text_size);
  if (case_sensitive || query->alternate[0] == query->exact[0]) return exact;
  size_t alternate_size = exact ? (size_t)(exact - text) : text_size;
  const char *alternate =
      memchr(text, query->alternate[0], alternate_size);
  return alternate ? alternate : exact;
}

/* FIRST_INDEX is a verified match for query byte zero. */
static inline bool fzf_ascii_plan_ordered_after_first(
    const char *text, size_t text_size, const fzf_ascii_query_plan_t *plan,
    size_t first_index) {
  size_t pattern_index = 1;
  size_t offset = first_index + 1;

  /* Dense refinements often preserve a contiguous prefix.  Consume it with
     the scalar path before paying any vector setup cost. */
  while (pattern_index < plan->pattern_size && offset < text_size &&
         fzf_ascii_plan_byte_matches((uint8_t)text[offset],
                                     &plan->bytes[pattern_index],
                                     plan->case_sensitive)) {
    pattern_index++;
    offset++;
  }
  if (pattern_index == plan->pattern_size) return true;

  bool use_vectors = plan->pattern_size >= FZF_SIMD_FUZZY_MIN_PATTERN &&
                     text_size >= FZF_SIMD_FUZZY_MIN_TEXT &&
                     plan->pattern_size - pattern_index > 2;
  while (use_vectors && pattern_index < plan->pattern_size &&
         offset + FZF_SIMD_LANES <= text_size) {
    fzf_simd_chunk_t chunk = fzf_simd_load(text + offset);
    uint16_t available = UINT16_MAX;
    while (pattern_index < plan->pattern_size) {
      uint16_t matches = (uint16_t)(
          fzf_simd_occurrences(chunk, &plan->bytes[pattern_index],
                               plan->case_sensitive) &
          available);
      if (matches == 0) break;
      unsigned int lane = fzf_simd_first_lane(matches);
      pattern_index++;
      available = (uint16_t)(UINT32_C(0xffff) << (lane + 1));
    }
    offset += FZF_SIMD_LANES;
  }

  while (pattern_index < plan->pattern_size) {
    const char *match = fzf_ascii_plan_find_byte(
        text + offset, text_size - offset, &plan->bytes[pattern_index],
        plan->case_sensitive);
    if (!match) return false;
    offset = (size_t)(match - text) + 1;
    pattern_index++;
  }
  return true;
}

static inline bool fzf_ascii_plan_matches_at(
    const char *text, const fzf_ascii_query_plan_t *plan) {
  if (plan->case_sensitive)
    return memcmp(text, plan->pattern, plan->pattern_size) == 0;
  for (size_t i = 0; i < plan->pattern_size; i++)
    if (!fzf_ascii_plan_byte_matches((uint8_t)text[i], &plan->bytes[i],
                                     plan->case_sensitive))
      return false;
  return true;
}

/* Return the first exact occurrence at or after FROM, or SIZE_MAX. */
static inline size_t fzf_ascii_plan_find_exact(
    const char *text, size_t text_size, const fzf_ascii_query_plan_t *plan,
    size_t from) {
  size_t pattern_size = plan->pattern_size;
  if (pattern_size == 0) return from <= text_size ? from : SIZE_MAX;
  if (text_size < pattern_size || from > text_size - pattern_size)
    return SIZE_MAX;

  /* Exact matches at byte zero are common in interactive completion.  Avoid
     vector setup.  Case-sensitive memcmp is already highly optimized; the
     folded path verifies the full literal only after both seeds agree. */
  if (from == 0 && pattern_size >= 2) {
    if (plan->case_sensitive) {
      if (memcmp(text, plan->pattern, pattern_size) == 0) return 0;
    } else if (
        fzf_ascii_plan_byte_matches(
            (uint8_t)text[plan->seed_a_offset],
            &plan->bytes[plan->seed_a_offset], false) &&
        fzf_ascii_plan_byte_matches(
            (uint8_t)text[plan->seed_b_offset],
            &plan->bytes[plan->seed_b_offset], false) &&
        fzf_ascii_plan_matches_at(text, plan)) {
      return 0;
    }
  }

  size_t last_start = text_size - pattern_size;
  size_t starts = last_start - from + 1;
  if (pattern_size >= 2 && starts >= FZF_SIMD_LITERAL_MIN_STARTS) {
    size_t start = from;
    while (start + FZF_SIMD_LANES - 1 <= last_start) {
      fzf_simd_chunk_t first =
          fzf_simd_load(text + start + plan->seed_a_offset);
      uint16_t hits = fzf_simd_occurrences(
          first, &plan->bytes[plan->seed_a_offset], plan->case_sensitive);
      if (hits != 0 && plan->seed_a_offset != plan->seed_b_offset) {
        fzf_simd_chunk_t second =
            fzf_simd_load(text + start + plan->seed_b_offset);
        hits = (uint16_t)(hits & fzf_simd_occurrences(
                                    second, &plan->bytes[plan->seed_b_offset],
                                    plan->case_sensitive));
      }
      while (hits != 0) {
        unsigned int lane = fzf_simd_first_lane(hits);
        size_t position = start + lane;
        if (fzf_ascii_plan_matches_at(text + position, plan)) return position;
        hits = (uint16_t)(hits & (uint16_t)(hits - 1));
      }
      start += FZF_SIMD_LANES;
    }
    from = start;
  }

  for (size_t position = from; position <= last_start; position++)
    if (fzf_ascii_plan_matches_at(text + position, plan)) return position;
  return SIZE_MAX;
}

#endif /* FZF_HAVE_SIMD_PREFILTER */
#endif /* FZF_SIMD_PREFILTER_H_ */
