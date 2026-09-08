// SPDX-License-Identifier: MIT
/* A short local A/B benchmark for one candidate's score+positions path.
   This is a development probe, not a substitute for the external holdout. */

#include "fzf.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct {
  const char *label;
  const char *text;
  const char *query;
  fzf_case_types case_mode;
  bool fuzzy;
  bool matches;
} bench_case_t;

static volatile uint64_t checksum;

static uint64_t monotonic_ns(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) abort();
  return (uint64_t)now.tv_sec * UINT64_C(1000000000) +
         (uint64_t)now.tv_nsec;
}

static void consume(int32_t score, const fzf_position_t *positions) {
  uint64_t value = (uint32_t)score;
  if (positions) {
    value += positions->size;
    if (positions->size) value += positions->data[positions->size - 1];
  }
  checksum += value;
}

static void run_legacy(size_t rounds, const char *text,
                       fzf_pattern_t *pattern, fzf_slab_t *slab) {
  for (size_t i = 0; i < rounds; i++) {
    int32_t score = fzf_get_score(text, pattern, slab);
    fzf_position_t *positions =
        score > 0 ? fzf_get_positions(text, pattern, slab) : NULL;
    consume(score, positions);
    fzf_free_positions(positions);
  }
}

static void run_combined(size_t rounds, const char *text,
                         fzf_pattern_t *pattern, fzf_slab_t *slab) {
  for (size_t i = 0; i < rounds; i++) {
    fzf_position_t *positions = NULL;
    int32_t score =
        fzf_get_score_positions(text, pattern, slab, &positions);
    consume(score, positions);
    fzf_free_positions(positions);
  }
}

static int compare_double(const void *left, const void *right) {
  double a = *(const double *)left;
  double b = *(const double *)right;
  return (a > b) - (a < b);
}

static bool positions_equal(const fzf_position_t *a,
                            const fzf_position_t *b) {
  if (!a || !b) return a == b;
  return a->size == b->size &&
         (a->size == 0 ||
          memcmp(a->data, b->data, a->size * sizeof *a->data) == 0);
}

static int bench_one(const bench_case_t *item, size_t rounds,
                     size_t samples) {
  char *mutable_query = strdup(item->query);
  fzf_pattern_t *pattern = fzf_parse_pattern(
      item->case_mode, false, mutable_query, item->fuzzy);
  fzf_slab_t *slab = fzf_make_default_slab();
  if (!mutable_query || !pattern || !slab) {
    fprintf(stderr, "setup failed for %s\n", item->label);
    free(mutable_query);
    fzf_free_pattern(pattern);
    fzf_free_slab(slab);
    return 1;
  }

  int32_t legacy_score = fzf_get_score(item->text, pattern, slab);
  fzf_position_t *legacy_positions =
      fzf_get_positions(item->text, pattern, slab);
  fzf_position_t *combined_positions = NULL;
  int32_t combined_score = fzf_get_score_positions(
      item->text, pattern, slab, &combined_positions);
  bool legacy_matches = legacy_score > 0;
  if (legacy_matches != item->matches ||
      (item->matches && (!legacy_positions || legacy_positions->size == 0))) {
    fprintf(stderr, "unexpected benchmark membership for %s\n", item->label);
    fzf_free_positions(combined_positions);
    fzf_free_positions(legacy_positions);
    free(mutable_query);
    fzf_free_pattern(pattern);
    fzf_free_slab(slab);
    return 1;
  }
  if (legacy_score != combined_score ||
      !positions_equal(legacy_positions, combined_positions)) {
    fprintf(stderr, "result mismatch for %s\n", item->label);
    fzf_free_positions(combined_positions);
    fzf_free_positions(legacy_positions);
    free(mutable_query);
    fzf_free_pattern(pattern);
    fzf_free_slab(slab);
    return 1;
  }
  fzf_free_positions(combined_positions);
  fzf_free_positions(legacy_positions);

  run_legacy(64, item->text, pattern, slab);
  run_combined(64, item->text, pattern, slab);

  double *legacy = calloc(samples, sizeof *legacy);
  double *combined = calloc(samples, sizeof *combined);
  if (!legacy || !combined) abort();
  for (size_t sample = 0; sample < samples; sample++) {
    uint64_t start;
    if ((sample & 1) == 0) {
      start = monotonic_ns();
      run_legacy(rounds, item->text, pattern, slab);
      legacy[sample] = (double)(monotonic_ns() - start) / rounds;
      start = monotonic_ns();
      run_combined(rounds, item->text, pattern, slab);
      combined[sample] = (double)(monotonic_ns() - start) / rounds;
    } else {
      start = monotonic_ns();
      run_combined(rounds, item->text, pattern, slab);
      combined[sample] = (double)(monotonic_ns() - start) / rounds;
      start = monotonic_ns();
      run_legacy(rounds, item->text, pattern, slab);
      legacy[sample] = (double)(monotonic_ns() - start) / rounds;
    }
  }
  qsort(legacy, samples, sizeof *legacy, compare_double);
  qsort(combined, samples, sizeof *combined, compare_double);
  double old_ns = legacy[samples / 2];
  double new_ns = combined[samples / 2];
  printf("%-14s %10.1f %10.1f %8.2fx %7.1f%%\n", item->label,
         old_ns, new_ns, old_ns / new_ns, 100.0 * (old_ns - new_ns) / old_ns);

  free(combined);
  free(legacy);
  free(mutable_query);
  fzf_free_pattern(pattern);
  fzf_free_slab(slab);
  return 0;
}

int main(int argc, char **argv) {
  size_t rounds = argc > 1 ? strtoul(argv[1], NULL, 10) : 4000;
  size_t samples = argc > 2 ? strtoul(argv[2], NULL, 10) : 9;
  if (rounds == 0 || samples == 0 || (samples & 1) == 0) {
    fprintf(stderr, "rounds and odd sample count must be positive\n");
    return 2;
  }

  static const bench_case_t cases[] = {
    {"emacs", "/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp/bytecomp.el.gz",
     "emacs", CaseIgnore, true, true},
    {"emacs miss", "/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp/bytecomp.el.gz",
     "zqv", CaseIgnore, true, false},
    {"e-macs", "/Users/duc/src/emacs/modules/fzf-native/src/completion-at-point-functions.el",
     "e-macs", CaseIgnore, true, true},
    {"simple miss", "/Users/duc/src/emacs/modules/fzf-native/src/completion-at-point-functions.el",
     "zzzz", CaseIgnore, true, false},
    {"long sparse", "archive/2026/snapshots/editor-extensions/native/module/commands-and-completion-sources.el",
     "e-mod-src", CaseIgnore, true, true},
    {"extended", "src/editor/emacs/modules/fzf-native-module.c",
     "emacs module", CaseIgnore, true, true},
    {"late AND miss", "src/editor/emacs/modules/fzf-native-module.c",
     "emacs impossible$", CaseIgnore, true, false},
    {"CJK", "项目/编辑器/路径/原生模糊匹配组件-123/文件名.el",
     "组件", CaseSmart, true, true},
    {"case-fold", "packages/CAFÉ-tools/src/CompletionMatcher.el",
     "café", CaseIgnore, true, true},
  };

  printf("score+positions microbenchmark (%zu rounds, %zu samples; median)\n",
         rounds, samples);
  printf("%-14s %10s %10s %8s %8s\n", "case", "legacy ns", "one-pass ns",
         "speedup", "saved");
  for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
    if (bench_one(&cases[i], rounds, samples)) return 1;
  }
  printf("checksum=%" PRIu64 "\n", checksum);
  return 0;
}
