// SPDX-License-Identifier: MIT
/*
 * Short, deterministic matcher probe for the Chromium, Arabic, and Korean
 * real-data holdout shapes, the first-byte ASCII hit path used by short
 * incremental queries, and UTF-8 inputs that exceed the v2 slab.  It is
 * deliberately synthetic: its purpose is to isolate core scorer costs without
 * rereading or sorting a multi-million-line corpus.  Results are provisional
 * and must not be presented as holdout data.
 *
 * Build from the repository root:
 *
 *   cc -std=gnu11 -O3 -DNDEBUG -I. -Iutf8proc-2.10.0 \
 *      -o build/core-hotpath-probe benchmarks/core-hotpath-probe.c \
 *      fzf.c utf8proc-2.10.0/utf8proc.c
 */

#include "fzf.h"
#include "fzf-private.h"
#include "utf8proc-2.10.0/utf8proc.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define PROBE_ITEMS 32768u
#ifndef PROBE_SAMPLES
#define PROBE_SAMPLES 11u
#endif
#define ASCII_BYTES 72u
#define UNICODE_CODEPOINTS 36u
#define FALLBACK_ITEMS 512u
#define FALLBACK_CODEPOINTS 4096u
#define FALLBACK_QUERY_CODEPOINTS 26u

typedef struct {
  char *data;
  size_t size;
  bool ascii;
} probe_item_t;

typedef struct {
  uint64_t first;
  uint64_t second;
} probe_fingerprint_t;

typedef struct {
  const char *name;
  const char *query;
  probe_item_t *items;
  size_t item_count;
  probe_fingerprint_t expected;
} probe_case_t;

static volatile uint64_t score_sink;

static uint64_t now_ns(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) abort();
  return (uint64_t)ts.tv_sec * UINT64_C(1000000000) + (uint64_t)ts.tv_nsec;
}

static int compare_u64(const void *left, const void *right) {
  uint64_t a = *(const uint64_t *)left;
  uint64_t b = *(const uint64_t *)right;
  return (a > b) - (a < b);
}

static uint64_t rotate_left_u64(uint64_t value, unsigned int shift) {
  return (value << shift) | (value >> (64 - shift));
}

static uint64_t avalanche_u64(uint64_t value) {
  value ^= value >> 30;
  value *= UINT64_C(0xbf58476d1ce4e5b9);
  value ^= value >> 27;
  value *= UINT64_C(0x94d049bb133111eb);
  return value ^ (value >> 31);
}

/* Keep two independently seeded, order-sensitive lanes.  Each item mixes its
   index, exact signed score bits, and membership verdict.  This makes swaps,
   score changes, and match-set changes visible without copying the scorer
   into the benchmark oracle. */
static void fingerprint_score(probe_fingerprint_t *fingerprint,
                              size_t item_index, int32_t score) {
  uint64_t index_bits = (uint64_t)item_index;
  uint64_t score_bits = (uint64_t)(uint32_t)score;
  uint64_t membership = score > 0 ? 1 : 0;
  uint64_t tuple = avalanche_u64(index_bits ^ UINT64_C(0x243f6a8885a308d3));
  tuple ^= rotate_left_u64(
      avalanche_u64(score_bits ^ UINT64_C(0x13198a2e03707344)), 19);
  tuple ^= avalanche_u64(membership ^ UINT64_C(0xa4093822299f31d0));
  fingerprint->first = avalanche_u64(
      fingerprint->first ^ tuple ^ UINT64_C(0x082efa98ec4e6c89));
  fingerprint->second = avalanche_u64(
      fingerprint->second + rotate_left_u64(tuple, 31) +
      index_bits * UINT64_C(0x9e3779b97f4a7c15));
}

static bool fingerprint_equal(probe_fingerprint_t left,
                              probe_fingerprint_t right) {
  return left.first == right.first && left.second == right.second;
}

static void put_cp(char *out, size_t *offset, utf8proc_int32_t cp) {
  utf8proc_ssize_t width = utf8proc_encode_char(
      cp, (utf8proc_uint8_t *)out + *offset);
  if (width <= 0) abort();
  *offset += (size_t)width;
}

static probe_item_t *make_ascii_items(void) {
  probe_item_t *items = calloc(PROBE_ITEMS, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < PROBE_ITEMS; i++) {
    char *text = malloc(ASCII_BYTES + 1);
    if (!text) abort();
    for (size_t j = 0; j < ASCII_BYTES; j++)
      text[j] = "abcdefg_/0123456789"[(i * 7 + j * 11) % 19];

    /* Eight percent match.  The larger partial cohort exercises the common
       progressive-rejection shape of the Chromium "linux" query. */
    size_t kind = i % 100;
    if (kind < 8) {
      memcpy(text + 9, "l-aa-i-bb-n-cc-u-dd-x", 21);
    } else if (kind < 68) {
      memcpy(text + 9, "l-aa-i-bb-n-cc-u-dd-q", 21);
    }
    text[ASCII_BYTES] = '\0';
    items[i] = (probe_item_t){text, ASCII_BYTES, true};
  }
  return items;
}

static probe_item_t *make_ascii_early_hit_items(void) {
  probe_item_t *items = calloc(PROBE_ITEMS, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < PROBE_ITEMS; i++) {
    char *text = malloc(ASCII_BYTES + 1);
    if (!text) abort();
    text[0] = 'z';
    for (size_t j = 1; j < ASCII_BYTES; j++)
      text[j] = "abcdefg_/0123456789"[(i * 7 + j * 11) % 19];
    text[ASCII_BYTES] = '\0';
    items[i] = (probe_item_t){text, ASCII_BYTES, true};
  }
  return items;
}

/* One-character completion symbols are small but miss-heavy.  Cycling over
   the alphabet gives query "z" one hit in 26 candidates and prevents an
   all-hit microbenchmark from hiding fixed dispatch overhead on misses. */
static probe_item_t *make_ascii_one_byte_items(void) {
  probe_item_t *items = calloc(PROBE_ITEMS, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < PROBE_ITEMS; i++) {
    char *text = malloc(2);
    if (!text) abort();
    text[0] = (char)('a' + i % 26);
    text[1] = '\0';
    items[i] = (probe_item_t){text, 1, true};
  }
  return items;
}

static probe_item_t *make_unicode_items(utf8proc_int32_t base,
                                        utf8proc_int32_t first,
                                        utf8proc_int32_t second) {
  probe_item_t *items = calloc(PROBE_ITEMS, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < PROBE_ITEMS; i++) {
    /* Four bytes per codepoint is an upper bound, plus the terminator. */
    char *text = malloc(UNICODE_CODEPOINTS * 4 + 1);
    if (!text) abort();
    size_t offset = 0;
    size_t kind = i % 100;
    for (size_t j = 0; j < UNICODE_CODEPOINTS; j++) {
      utf8proc_int32_t cp = base + (utf8proc_int32_t)((i + j * 7) % 24);
      if (cp == first || cp == second) cp = base;
      if (j == 8 && kind < 68) cp = first;
      if (j == 25 && kind < 8) cp = second;
      put_cp(text, &offset, cp);
    }
    text[offset] = '\0';
    items[i] = (probe_item_t){text, offset, false};
  }
  return items;
}

/* A hit-heavy counterweight to the miss-dominated holdout shapes above.
   Every query matches only at the final codepoint.  This catches a full extra
   candidate decode hidden behind otherwise valuable early rejection. */
static probe_item_t *make_unicode_late_hit_items(utf8proc_int32_t base,
                                                utf8proc_int32_t query) {
  probe_item_t *items = calloc(PROBE_ITEMS, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < PROBE_ITEMS; i++) {
    char *text = malloc(UNICODE_CODEPOINTS * 4 + 1);
    if (!text) abort();
    size_t offset = 0;
    for (size_t j = 0; j < UNICODE_CODEPOINTS; j++) {
      utf8proc_int32_t cp = base + (utf8proc_int32_t)((i + j * 7) % 24);
      if (cp == query) cp = base;
      if (j + 1 == UNICODE_CODEPOINTS) cp = query;
      put_cp(text, &offset, cp);
    }
    text[offset] = '\0';
    items[i] = (probe_item_t){text, offset, false};
  }
  return items;
}

static probe_item_t *make_unicode_v1_fallback_items(bool early) {
  probe_item_t *items = calloc(FALLBACK_ITEMS, sizeof *items);
  if (!items) abort();
  for (size_t i = 0; i < FALLBACK_ITEMS; i++) {
    char *text = malloc(FALLBACK_CODEPOINTS * 3 + 1);
    if (!text) abort();
    size_t offset = 0;
    for (size_t j = 0; j < FALLBACK_CODEPOINTS; j++) {
      bool query_slot = early ? j < FALLBACK_QUERY_CODEPOINTS
                              : j >= FALLBACK_CODEPOINTS -
                                         FALLBACK_QUERY_CODEPOINTS;
      if (query_slot) {
        size_t query_index = early ? j
                                   : j - (FALLBACK_CODEPOINTS -
                                          FALLBACK_QUERY_CODEPOINTS);
        text[offset++] = (char)('a' + query_index);
      } else {
        put_cp(text, &offset,
               0x4E00 + (utf8proc_int32_t)((i + j * 7) % 24));
      }
    }
    text[offset] = '\0';
    items[i] = (probe_item_t){text, offset, false};
  }
  return items;
}

static probe_fingerprint_t fingerprint_case(
    const probe_case_t *probe, fzf_pattern_t *pattern, fzf_slab_t *slab) {
  probe_fingerprint_t fingerprint = {
      UINT64_C(0x452821e638d01377),
      UINT64_C(0xbe5466cf34e90c6c),
  };
  for (size_t i = 0; i < probe->item_count; i++) {
    const probe_item_t *item = &probe->items[i];
    int32_t score = fzf_get_score_bytes_preclassified(
        item->data, item->size, item->ascii, pattern, slab);
    if (fzf_allocation_failed()) abort();
#ifdef FZF_CORE_HOTPATH_FAULT_SCORE
    score = (int32_t)((uint32_t)score + 1);
#endif
    fingerprint_score(&fingerprint, i, score);
  }
  return fingerprint;
}

static bool verify_case(const probe_case_t *probe) {
  char query[32];
  size_t query_size = strlen(probe->query);
  if (query_size >= sizeof query) abort();
  memcpy(query, probe->query, query_size + 1);
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, true, query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  if (!pattern || !slab) abort();

  probe_fingerprint_t actual = fingerprint_case(probe, pattern, slab);
  bool valid = fingerprint_equal(actual, probe->expected);
  if (!valid) {
    fprintf(stderr,
            "core-hotpath fingerprint mismatch for %s: "
            "expected=%016" PRIx64 "%016" PRIx64 " "
            "actual=%016" PRIx64 "%016" PRIx64 "\n",
            probe->name, probe->expected.first, probe->expected.second,
            actual.first, actual.second);
  }
  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
  return valid;
}

static uint64_t score_once(const probe_case_t *probe, fzf_pattern_t *pattern,
                           fzf_slab_t *slab) {
  uint64_t checksum = 0;
  uint64_t start = now_ns();
  for (size_t i = 0; i < probe->item_count; i++) {
    const probe_item_t *item = &probe->items[i];
    checksum += (uint32_t)fzf_get_score_bytes_preclassified(
        item->data, item->size, item->ascii, pattern, slab);
    if (fzf_allocation_failed()) abort();
  }
  uint64_t elapsed = now_ns() - start;
  score_sink += checksum;
  return elapsed;
}

static void run_case(const probe_case_t *probe) {
  char query[32];
  size_t query_size = strlen(probe->query);
  if (query_size >= sizeof query) abort();
  memcpy(query, probe->query, query_size + 1);
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, true, query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  if (!pattern || !slab) abort();

  /* Populate retained slab scratch and warm instruction/data caches. */
  for (size_t i = 0; i < 3; i++) (void)score_once(probe, pattern, slab);

  uint64_t samples[PROBE_SAMPLES];
  for (size_t i = 0; i < PROBE_SAMPLES; i++)
    samples[i] = score_once(probe, pattern, slab);
  qsort(samples, PROBE_SAMPLES, sizeof samples[0], compare_u64);
  uint64_t median = samples[PROBE_SAMPLES / 2];
  printf("%-8s %8.3f ms  %7.2f ns/item  checksum=%" PRIu64 "\n",
         probe->name, (double)median / 1e6,
         (double)median / (double)probe->item_count, score_sink);

  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
}

static void free_items(probe_item_t *items, size_t item_count) {
  for (size_t i = 0; i < item_count; i++) free(items[i].data);
  free(items);
}

int main(void) {
  probe_case_t probes[] = {
      {"EarlyASCII", "z", make_ascii_early_hit_items(), PROBE_ITEMS,
       {UINT64_C(0x95c5e246fda7e865), UINT64_C(0x39111aa592e0d90e)}},
      {"OneByte", "z", make_ascii_one_byte_items(), PROBE_ITEMS,
       {UINT64_C(0x1d5da984442dc286), UINT64_C(0x290addec25f9fb52)}},
      {"Chromium", "linux", make_ascii_items(), PROBE_ITEMS,
       {UINT64_C(0x9ed9b37a1b7f2f03), UINT64_C(0xc448dc43cea84dca)}},
      {"Arabic", "\xD8\xA5\xD9\x86",
       make_unicode_items(0x0620, 0x0625, 0x0646), PROBE_ITEMS,
       {UINT64_C(0xbe61aa8e8f2cce1a), UINT64_C(0x7a752bfa868ee773)}},
      {"Korean", "\xEB\x8B\x88\xEB\x8B\xA4",
       make_unicode_items(0xAC00, 0xB2C8, 0xB2E4), PROBE_ITEMS,
       {UINT64_C(0xbe61aa8e8f2cce1a), UINT64_C(0x7a752bfa868ee773)}},
      {"UTF8-hit", "\xE7\x95\x8C",
       make_unicode_late_hit_items(0x4E00, 0x754C), PROBE_ITEMS,
       {UINT64_C(0x045ad257d95f42ef), UINT64_C(0xb2a0cde59c721518)}},
      {"V1-early", "abcdefghijklmnopqrstuvwxyz",
       make_unicode_v1_fallback_items(true), FALLBACK_ITEMS,
       {UINT64_C(0xb2fafe30d7bfd1d8), UINT64_C(0x45b8325332b02864)}},
      {"V1-late", "abcdefghijklmnopqrstuvwxyz",
       make_unicode_v1_fallback_items(false), FALLBACK_ITEMS,
       {UINT64_C(0x4a9740c54a846c31), UINT64_C(0x7dd7dc4c3d40b49e)}},
  };

  for (size_t i = 0; i < sizeof probes / sizeof probes[0]; i++) {
    if (!verify_case(&probes[i])) {
      for (size_t j = 0; j < sizeof probes / sizeof probes[0]; j++)
        free_items(probes[j].items, probes[j].item_count);
      return 1;
    }
  }
  for (size_t i = 0; i < sizeof probes / sizeof probes[0]; i++)
    run_case(&probes[i]);
  for (size_t i = 0; i < sizeof probes / sizeof probes[0]; i++)
    free_items(probes[i].items, probes[i].item_count);
  return 0;
}
