// SPDX-License-Identifier: MIT
/*
 * Measure the fixed work around the scorer on deterministic synthetic ASCII
 * misses.  This is not a corpus benchmark.  It separates the public C-string
 * entry point from the bounded and preclassified entry points so benchmark
 * adapters do not accidentally attribute candidate metadata work to the
 * matching algorithm.
 */

#include "../fzf.h"
#include "../fzf-private.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ITEMS 100000u
#define SAMPLES 15u

typedef struct {
  char *text;
  size_t length;
} item_t;

typedef enum {
  API_C_STRING,
  API_BOUNDED,
  API_PRECLASSIFIED,
} api_t;

static volatile uint64_t result_sink;

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

static item_t *make_items(size_t length, bool progressive) {
  item_t *items = calloc(ITEMS, sizeof *items);
  if (!items) abort();
  static const char alphabet[] = "acgijklmnoqrstuvwxyz0123456789_/";
  const size_t alphabet_length = sizeof alphabet - 1;
  for (size_t i = 0; i < ITEMS; i++) {
    char *text = malloc(length + 1);
    if (!text) abort();
    for (size_t j = 0; j < length; j++)
      text[j] = alphabet[(i * 17 + j * 29) % alphabet_length];
    if (progressive && length >= 8)
      memcpy(text + length - 8, "deadbeex", 8);
    text[length] = '\0';
    items[i] = (item_t){text, length};
  }
  return items;
}

static uint64_t score_once(api_t api, const item_t *items,
                           fzf_pattern_t *pattern, fzf_slab_t *slab,
                           uint64_t *checksum) {
  uint64_t sum = 0;
  uint64_t start = now_ns();
  for (size_t i = 0; i < ITEMS; i++) {
    int32_t score;
    if (api == API_C_STRING)
      score = fzf_get_score(items[i].text, pattern, slab);
    else if (api == API_BOUNDED)
      score = fzf_get_score_bytes(
          items[i].text, items[i].length, pattern, slab);
    else
      score = fzf_get_score_bytes_preclassified(
          items[i].text, items[i].length, true, pattern, slab);
    if (fzf_allocation_failed()) abort();
    sum = sum * UINT64_C(0x9e3779b185ebca87) + (uint32_t)score + i;
  }
  uint64_t elapsed = now_ns() - start;
  result_sink ^= sum;
  *checksum = sum;
  return elapsed;
}

static void print_api(const char *shape, api_t api,
                      uint64_t samples[SAMPLES], uint64_t checksum) {
  static const char *names[] = {"c-string", "bounded", "preclassified"};
  printf("%-12s %-13s %8.3f ms %7.2f ns/item checksum=%016" PRIx64 "\n",
         shape, names[api], (double)samples[SAMPLES / 2] / 1e6,
         (double)samples[SAMPLES / 2] / ITEMS, checksum);
}

static void run_shape(const char *shape, size_t length, bool progressive) {
  item_t *items = make_items(length, progressive);
  char query[] = "deadbeef";
  fzf_pattern_t *pattern = fzf_parse_pattern(CaseIgnore, false, query, true);
  fzf_slab_t *slab = fzf_make_default_slab();
  if (!pattern || !slab) abort();

  uint64_t expected_checksum = 0;
  (void)score_once(
      API_PRECLASSIFIED, items, pattern, slab, &expected_checksum);

  uint64_t samples[3][SAMPLES];
  uint64_t checksums[3] = {0};
  for (api_t api = API_C_STRING; api <= API_PRECLASSIFIED; api++) {
    for (size_t warmup = 0; warmup < 3; warmup++)
      (void)score_once(api, items, pattern, slab, &checksums[api]);
    if (checksums[api] != expected_checksum) abort();
  }
  /* Rotate the order on every sample to reduce frequency and temperature
     drift between the three API paths. */
  for (size_t sample = 0; sample < SAMPLES; sample++) {
    for (size_t offset = 0; offset < 3; offset++) {
      api_t api = (api_t)((sample + offset) % 3);
      samples[api][sample] = score_once(
          api, items, pattern, slab, &checksums[api]);
      if (checksums[api] != expected_checksum) abort();
    }
  }
  for (api_t api = API_C_STRING; api <= API_PRECLASSIFIED; api++) {
    qsort(samples[api], SAMPLES, sizeof samples[api][0], compare_u64);
    print_api(shape, api, samples[api], checksums[api]);
  }

  fzf_free_slab(slab);
  fzf_free_pattern(pattern);
  for (size_t i = 0; i < ITEMS; i++) free(items[i].text);
  free(items);
}

int main(void) {
  run_shape("early-32", 32, false);
  run_shape("late-32", 32, true);
  run_shape("early-128", 128, false);
  run_shape("late-128", 128, true);
  return 0;
}
