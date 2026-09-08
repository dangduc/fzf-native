// SPDX-License-Identifier: MIT
/*
 * Short, deterministic matcher probe for the Chromium, Arabic, and Korean
 * real-data holdout shapes, plus the first-byte ASCII hit path used by short
 * incremental queries.  It is deliberately synthetic: its purpose is to
 * isolate core scorer costs without rereading or sorting a multi-million-line
 * corpus.  Results are provisional and must not be presented as holdout data.
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

typedef struct {
  char *data;
  size_t size;
  bool ascii;
} probe_item_t;

typedef struct {
  const char *name;
  const char *query;
  probe_item_t *items;
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

static uint64_t score_once(const probe_case_t *probe, fzf_pattern_t *pattern,
                           fzf_slab_t *slab) {
  uint64_t checksum = 0;
  uint64_t start = now_ns();
  for (size_t i = 0; i < PROBE_ITEMS; i++) {
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
         (double)median / (double)PROBE_ITEMS, score_sink);

  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
}

static void free_items(probe_item_t *items) {
  for (size_t i = 0; i < PROBE_ITEMS; i++) free(items[i].data);
  free(items);
}

int main(void) {
  probe_case_t probes[] = {
      {"EarlyASCII", "z", make_ascii_early_hit_items()},
      {"Chromium", "linux", make_ascii_items()},
      {"Arabic", "\xD8\xA5\xD9\x86",
       make_unicode_items(0x0620, 0x0625, 0x0646)},
      {"Korean", "\xEB\x8B\x88\xEB\x8B\xA4",
       make_unicode_items(0xAC00, 0xB2C8, 0xB2E4)},
  };

  for (size_t i = 0; i < sizeof probes / sizeof probes[0]; i++)
    run_case(&probes[i]);
  for (size_t i = 0; i < sizeof probes / sizeof probes[0]; i++)
    free_items(probes[i].items);
  return 0;
}
