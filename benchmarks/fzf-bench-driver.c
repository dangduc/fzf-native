/* SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Standalone fzf-native equivalent of junegunn/fzf's --bench scan boundary.
 *
 * fzf builds the pattern and ingests the input before timing.  Each timed
 * matcher.scan call divides 1024-item chunks among workers, collects every
 * match in a worker-local result list, sorts each list, constructs a lazy
 * merger, and reads only Merger.Length().  It does not materialize the global
 * order.  This driver follows that boundary: the final k-way merge is used
 * only after timing to produce a deterministic semantic checksum.
 *
 * Persistent pthreads stand in for Go's persistent runtime threads and its
 * per-scan goroutines.  Match buffers are discarded between samples outside
 * the measured interval, while per-worker slabs and radix scratch persist as
 * they do in fzf's Matcher.  Candidate length and ASCII classification are
 * prepared during ingestion; fzf-native has no public retained-rune candidate
 * representation, so non-ASCII scoring still decodes through its normal API.
 * fzf also performs cold ChunkCache bookkeeping inside Pattern.Match after
 * clearing that cache before each sample.  The native scorer has no analogous
 * cache interface, so this driver does not add a synthetic cache cost.
 * The --no-sort mode is a native diagnostic.  fzf filter mode forces sorting
 * for sortable patterns, even when its command line contains --no-sort.
 */

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "fzf-private.h"
#include "utf8proc.h"

#ifndef FZF_BENCH_CHUNK_SIZE
#define FZF_BENCH_CHUNK_SIZE 1024
#endif

typedef struct {
  const char *text;
  size_t length;
  uint32_t index;
  uint16_t rank_length;
  bool input_is_ascii;
  bool rank_length_known;
} BenchCandidate;

typedef struct {
  char *bytes;
  size_t byte_count;
  BenchCandidate *candidates;
  size_t count;
} BenchCorpus;

/* Keep the same 16-byte result footprint as fzf's Result on 64-bit hosts. */
typedef struct {
  const BenchCandidate *candidate;
  int32_t membership;
  uint16_t rank_score;
  uint16_t rank_length;
} BenchMatch;

typedef struct {
  BenchMatch *values;
  size_t count;
  size_t capacity;
} BenchMatchList;

typedef struct {
  BenchMatchList *lists;
  size_t list_count;
  size_t *cursors;
  size_t count;
  bool sorted;
  bool pass;
} BenchMerger;

typedef struct {
  BenchMerger *merger;
  BenchMerger *pass_merger;
} BenchScanResult;

struct BenchPool;

typedef struct {
  struct BenchPool *pool;
  pthread_t thread;
  fzf_slab_t *slab;
  BenchMatchList matches;
  BenchMatch *sort_scratch;
  size_t sort_scratch_capacity;
  uint64_t seen_epoch;
} BenchWorker;

typedef struct BenchPool {
  pthread_mutex_t mutex;
  pthread_cond_t start_cond;
  pthread_cond_t done_cond;
  BenchWorker *workers;
  size_t worker_count;
  size_t ready_count;
  size_t active_count;
  uint64_t epoch;
  bool stop;
  BenchCorpus *corpus;
  fzf_pattern_t *pattern;
  bool has_positive_term;
  bool sort_results;
  bool tiebreak_length;
  BenchScanResult result;
  _Atomic size_t next_chunk;
  _Atomic bool failed;
} BenchPool;

typedef struct {
  const char *query;
  uint64_t duration_ns;
  unsigned threads;
  bool normalize;
  bool sort_results;
  bool tiebreak_length;
  bool check_only;
  bool dump_results;
  bool dump_semantic;
  bool help;
} BenchOptions;

static uint64_t bench_now_ns(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) return 0;
  return (uint64_t)now.tv_sec * UINT64_C(1000000000) +
      (uint64_t)now.tv_nsec;
}

static void bench_usage(FILE *stream, const char *program) {
  fprintf(stream,
          "usage: %s --filter QUERY "
          "(--bench DURATION | --check | --dump-results | --dump-semantic) "
          "[--threads N] [--literal] "
          "[--sort | --no-sort] [--algo=v2] "
          "[--tiebreak=(length|index)]\n",
          program);
}

static bool bench_option_value(int argc, char **argv, int *index,
                               const char *name, const char **value) {
  size_t length = strlen(name);
  const char *argument = argv[*index];
  if (strcmp(argument, name) == 0) {
    if (*index + 1 >= argc) return false;
    *value = argv[++*index];
    return true;
  }
  if (strncmp(argument, name, length) == 0 && argument[length] == '=') {
    *value = argument + length + 1;
    return true;
  }
  return false;
}

static bool bench_parse_duration(const char *text, uint64_t *duration_ns) {
  errno = 0;
  char *suffix = NULL;
  long double value = strtold(text, &suffix);
  if (errno != 0 || suffix == text || !isfinite(value) || value <= 0.0)
    return false;

  long double multiplier;
  if (strcmp(suffix, "ns") == 0)
    multiplier = 1.0L;
  else if (strcmp(suffix, "us") == 0)
    multiplier = 1e3L;
  else if (strcmp(suffix, "ms") == 0)
    multiplier = 1e6L;
  else if (strcmp(suffix, "s") == 0)
    multiplier = 1e9L;
  else if (strcmp(suffix, "m") == 0)
    multiplier = 60e9L;
  else if (strcmp(suffix, "h") == 0)
    multiplier = 3600e9L;
  else
    return false;

  long double nanoseconds = value * multiplier;
  /* 2^64 is exact even when long double has only double precision. */
  if (!isfinite(nanoseconds) || nanoseconds < 1.0L ||
      nanoseconds >= 0x1p64L)
    return false;
  *duration_ns = (uint64_t)nanoseconds;
  return *duration_ns > 0;
}

static bool bench_parse_threads(const char *text, unsigned *threads) {
  if (!text[0] || text[0] == '-') return false;
  errno = 0;
  char *end = NULL;
  unsigned long value = strtoul(text, &end, 10);
  if (errno != 0 || *end != '\0' || value > UINT_MAX) return false;
  *threads = (unsigned)value;
  return true;
}

static unsigned bench_online_threads(void) {
  long value = sysconf(_SC_NPROCESSORS_ONLN);
  if (value <= 0) return 1;
  if ((unsigned long)value > UINT_MAX) return UINT_MAX;
  return (unsigned)value;
}

static bool bench_parse_options(int argc, char **argv,
                                BenchOptions *options) {
  *options = (BenchOptions){
      .normalize = true, .sort_results = true, .tiebreak_length = true};
  for (int i = 1; i < argc; i++) {
    const char *argument = argv[i];
    const char *value = NULL;
    if (strcmp(argument, "--help") == 0 || strcmp(argument, "-h") == 0) {
      options->help = true;
    } else if (strcmp(argument, "--check") == 0) {
      options->check_only = true;
    } else if (strcmp(argument, "--dump-results") == 0) {
      options->dump_results = true;
    } else if (strcmp(argument, "--dump-semantic") == 0) {
      options->dump_semantic = true;
    } else if (strcmp(argument, "--literal") == 0) {
      options->normalize = false;
    } else if (strcmp(argument, "--no-literal") == 0) {
      options->normalize = true;
    } else if (strcmp(argument, "--no-sort") == 0 ||
               strcmp(argument, "+s") == 0) {
      options->sort_results = false;
    } else if (strcmp(argument, "--sort") == 0 ||
               strcmp(argument, "-s") == 0) {
      options->sort_results = true;
    } else if (strcmp(argument, "-f") == 0) {
      if (++i >= argc) return false;
      options->query = argv[i];
    } else if (bench_option_value(
                   argc, argv, &i, "--filter", &value)) {
      options->query = value;
    } else if (bench_option_value(
                   argc, argv, &i, "--bench", &value)) {
      if (!bench_parse_duration(value, &options->duration_ns)) return false;
    } else if (bench_option_value(
                   argc, argv, &i, "--threads", &value)) {
      if (!bench_parse_threads(value, &options->threads)) return false;
    } else if (bench_option_value(
                   argc, argv, &i, "--algo", &value)) {
      if (strcmp(value, "v2") != 0) return false;
    } else if (bench_option_value(
                   argc, argv, &i, "--tiebreak", &value)) {
      if (strcmp(value, "index") == 0) {
        options->tiebreak_length = false;
      } else if (strcmp(value, "length") == 0) {
        options->tiebreak_length = true;
      } else {
        return false;
      }
    } else if (bench_option_value(
                   argc, argv, &i, "--scheme", &value)) {
      if (strcmp(value, "default") != 0) return false;
    } else {
      return false;
    }
  }

  if (options->help) return true;
  unsigned modes = (options->duration_ns > 0) + options->check_only +
      options->dump_results + options->dump_semantic;
  if (!options->query || modes != 1)
    return false;
  if (options->threads == 0) options->threads = bench_online_threads();
  return options->threads > 0;
}

static bool bench_read_all(FILE *stream, char **bytes, size_t *length) {
  char *buffer = NULL;
  size_t used = 0;
  size_t capacity = 0;
  for (;;) {
    if (capacity - used < 64 * 1024) {
      size_t next = capacity ? capacity * 2 : 128 * 1024;
      if (next < capacity || next > SIZE_MAX - 1) {
        free(buffer);
        return false;
      }
      char *grown = realloc(buffer, next + 1);
      if (!grown) {
        free(buffer);
        return false;
      }
      buffer = grown;
      capacity = next;
    }
    size_t amount = fread(buffer + used, 1, capacity - used, stream);
    used += amount;
    if (amount == 0) {
      if (ferror(stream)) {
        free(buffer);
        return false;
      }
      break;
    }
  }
  if (!buffer) {
    buffer = malloc(1);
    if (!buffer) return false;
  }
  buffer[used] = '\0';
  *bytes = buffer;
  *length = used;
  return true;
}

static bool bench_load_corpus(FILE *stream, BenchCorpus *corpus) {
  memset(corpus, 0, sizeof *corpus);
  if (!bench_read_all(stream, &corpus->bytes, &corpus->byte_count))
    return false;

  size_t count = 0;
  for (size_t i = 0; i < corpus->byte_count; i++)
    if (corpus->bytes[i] == '\n') count++;
  if (corpus->byte_count > 0 &&
      corpus->bytes[corpus->byte_count - 1] != '\n')
    count++;
  if (count > INT32_MAX || count > SIZE_MAX / sizeof *corpus->candidates)
    goto fail;

  corpus->candidates = count
      ? calloc(count, sizeof *corpus->candidates) : NULL;
  if (count && !corpus->candidates) goto fail;

  size_t start = 0;
  size_t candidate_index = 0;
  for (size_t i = 0; i <= corpus->byte_count; i++) {
    bool delimiter = i < corpus->byte_count && corpus->bytes[i] == '\n';
    bool final_line = i == corpus->byte_count && i > start;
    if (!delimiter && !final_line) continue;
    size_t length = i - start;
#ifdef _WIN32
    if (delimiter && length > 0 && corpus->bytes[i - 1] == '\r') length--;
#endif
    BenchCandidate *candidate = &corpus->candidates[candidate_index];
    *candidate = (BenchCandidate){
        .text = corpus->bytes + start,
        .length = length,
        .index = (uint32_t)candidate_index,
        .input_is_ascii = is_ascii_utf8proc(corpus->bytes + start, length),
    };
    candidate_index++;
    start = i + 1;
  }
  corpus->count = candidate_index;
  if (candidate_index == count) return true;

fail:
  free(corpus->candidates);
  free(corpus->bytes);
  memset(corpus, 0, sizeof *corpus);
  return false;
}

static void bench_free_corpus(BenchCorpus *corpus) {
  free(corpus->candidates);
  free(corpus->bytes);
  memset(corpus, 0, sizeof *corpus);
}

static uint16_t bench_rank_score(int64_t score) {
  if (score <= 0) return 0;
  return score > UINT16_MAX ? UINT16_MAX : (uint16_t)score;
}

static bool bench_rank_is_space(utf8proc_int32_t codepoint) {
  return (codepoint >= 0x09 && codepoint <= 0x0d) ||
      codepoint == 0x20 || codepoint == 0x85 || codepoint == 0xa0 ||
      codepoint == 0x1680 ||
      (codepoint >= 0x2000 && codepoint <= 0x200a) ||
      codepoint == 0x2028 || codepoint == 0x2029 ||
      codepoint == 0x202f || codepoint == 0x205f ||
      codepoint == 0x3000;
}

/* fzf computes Item.TrimLength lazily on the first matching scan and caches
   it on the item.  Each candidate belongs to one worker per scan, and scans
   do not overlap, so the equivalent per-candidate cache needs no lock. */
static uint16_t bench_rank_length(BenchCandidate *candidate) {
  if (candidate->rank_length_known) return candidate->rank_length;

  if (candidate->input_is_ascii) {
    size_t first = 0;
    size_t end = candidate->length;
    while (first < end &&
           bench_rank_is_space((unsigned char)candidate->text[first]))
      first++;
    while (end > first &&
           bench_rank_is_space((unsigned char)candidate->text[end - 1]))
      end--;
    size_t length = end - first;
    candidate->rank_length = length > UINT16_MAX
        ? UINT16_MAX : (uint16_t)length;
    candidate->rank_length_known = true;
    return candidate->rank_length;
  }

  size_t first_nonspace = SIZE_MAX;
  size_t last_nonspace = 0;
  size_t rune_index = 0;
  size_t offset = 0;
  while (offset < candidate->length) {
    utf8proc_int32_t codepoint = 0;
    utf8proc_ssize_t width = utf8proc_iterate(
        (const utf8proc_uint8_t *)candidate->text + offset,
        (utf8proc_ssize_t)(candidate->length - offset), &codepoint);
    if (width <= 0) {
      codepoint = (unsigned char)candidate->text[offset];
      width = 1;
    }
    if (!bench_rank_is_space(codepoint)) {
      if (first_nonspace == SIZE_MAX) first_nonspace = rune_index;
      last_nonspace = rune_index;
    }
    offset += (size_t)width;
    rune_index++;
  }
  size_t length = first_nonspace == SIZE_MAX
      ? 0 : last_nonspace - first_nonspace + 1;
  candidate->rank_length = length > UINT16_MAX
      ? UINT16_MAX : (uint16_t)length;
  candidate->rank_length_known = true;
  return candidate->rank_length;
}

static int bench_compare_matches(const void *left_value,
                                 const void *right_value) {
  const BenchMatch *left = left_value;
  const BenchMatch *right = right_value;
  if (left->rank_score != right->rank_score)
    return left->rank_score > right->rank_score ? -1 : 1;
  if (left->rank_length != right->rank_length)
    return left->rank_length < right->rank_length ? -1 : 1;
  if (left->candidate->index != right->candidate->index)
    return left->candidate->index < right->candidate->index ? -1 : 1;
  return 0;
}

static bool bench_match_list_append(BenchMatchList *list,
                                    BenchMatch match) {
  if (list->count == list->capacity) {
    size_t next = list->capacity ? list->capacity * 2 : 128;
    if (next < list->capacity || next > SIZE_MAX / sizeof *list->values)
      return false;
    BenchMatch *grown = realloc(list->values, next * sizeof *grown);
    if (!grown) return false;
    list->values = grown;
    list->capacity = next;
  }
  list->values[list->count++] = match;
  return true;
}

static uint32_t bench_sort_key(const BenchMatch *match) {
  return (uint32_t)match->rank_length |
      (uint32_t)(UINT16_MAX - match->rank_score) << 16;
}

/* fzf uses comparison sort below 128 results and an LSD radix sort above it.
   The default key is inverted score followed by trimmed rune length.  With
   --tiebreak=index, the length half is zero and stable input order supplies
   the final index tiebreak. */
static void bench_sort_matches(BenchWorker *worker) {
  BenchMatch *values = worker->matches.values;
  size_t count = worker->matches.count;
  if (count < 2) return;
  if (count < 128) {
    qsort(values, count, sizeof *values, bench_compare_matches);
    return;
  }

  if (worker->sort_scratch_capacity < count) {
    BenchMatch *grown = realloc(
        worker->sort_scratch, count * sizeof *worker->sort_scratch);
    if (!grown) {
      qsort(values, count, sizeof *values, bench_compare_matches);
      return;
    }
    worker->sort_scratch = grown;
    worker->sort_scratch_capacity = count;
  }

  uint32_t key_or = 0;
  for (size_t i = 0; i < count; i++)
    key_or |= bench_sort_key(&values[i]);

  BenchMatch *source = values;
  BenchMatch *destination = worker->sort_scratch;
  unsigned scatters = 0;
  for (unsigned pass = 0; pass < 4; pass++) {
    unsigned shift = pass * 8;
    if (((key_or >> shift) & 0xff) == 0) continue;
    size_t counts[256] = {0};
    for (size_t i = 0; i < count; i++) {
      uint32_t key = bench_sort_key(&source[i]);
      counts[(key >> shift) & 0xff]++;
    }
    uint32_t first_key = bench_sort_key(&source[0]);
    if (counts[(first_key >> shift) & 0xff] == count) continue;
    size_t offsets[256];
    offsets[0] = 0;
    for (size_t i = 1; i < 256; i++)
      offsets[i] = offsets[i - 1] + counts[i - 1];
    for (size_t i = 0; i < count; i++) {
      uint32_t key = bench_sort_key(&source[i]);
      destination[offsets[(key >> shift) & 0xff]++] = source[i];
    }
    BenchMatch *swap = source;
    source = destination;
    destination = swap;
    scatters++;
  }
  if (scatters & 1)
    memcpy(values, source, count * sizeof *values);
}

static void bench_worker_scan(BenchWorker *worker) {
  BenchPool *pool = worker->pool;
  worker->matches.count = 0;
  if (!worker->slab) {
    worker->slab = fzf_make_default_slab();
    if (!worker->slab || !fzf_slab_set_score_scheme(
                             worker->slab, FZF_SCORE_SCHEME_DEFAULT)) {
      atomic_store_explicit(&pool->failed, true, memory_order_relaxed);
      return;
    }
  }

  size_t chunk_count = (pool->corpus->count + FZF_BENCH_CHUNK_SIZE - 1) /
      FZF_BENCH_CHUNK_SIZE;
  for (;;) {
    size_t chunk = atomic_fetch_add_explicit(
        &pool->next_chunk, 1, memory_order_relaxed);
    if (chunk >= chunk_count) break;
    size_t first = chunk * FZF_BENCH_CHUNK_SIZE;
    size_t end = first + FZF_BENCH_CHUNK_SIZE;
    if (end > pool->corpus->count) end = pool->corpus->count;
    for (size_t i = first; i < end; i++) {
      BenchCandidate *candidate = &pool->corpus->candidates[i];
      fzf_score_bounds_t bounds;
      int32_t membership = fzf_get_score_with_bounds_bytes_preclassified(
          candidate->text, candidate->length, candidate->input_is_ascii,
          pool->pattern, worker->slab, &bounds);
      if (fzf_allocation_failed()) {
        atomic_store_explicit(&pool->failed, true, memory_order_relaxed);
        return;
      }
      /* The public return preserves membership with a positive sentinel.
         A v1 fallback can have a negative raw score.  Rank the raw aggregate,
         as fzf does, rather than adding sentinel-adjusted term scores. */
      if (membership > 0 && !bench_match_list_append(
                           &worker->matches,
                           (BenchMatch){
                               .candidate = candidate,
                               .membership = membership,
                               .rank_score = pool->has_positive_term
                                   ? bench_rank_score(bounds.raw_score) : 0,
                               .rank_length = pool->tiebreak_length
                                   ? bench_rank_length(candidate) : 0,
                           })) {
        atomic_store_explicit(&pool->failed, true, memory_order_relaxed);
        return;
      }
    }
  }
  if (pool->sort_results) bench_sort_matches(worker);
}

static void *bench_worker_main(void *data) {
  BenchWorker *worker = data;
  BenchPool *pool = worker->pool;
  pthread_mutex_lock(&pool->mutex);
  pool->ready_count++;
  pthread_cond_signal(&pool->done_cond);
  while (!pool->stop) {
    while (!pool->stop && worker->seen_epoch == pool->epoch)
      pthread_cond_wait(&pool->start_cond, &pool->mutex);
    if (pool->stop) break;
    worker->seen_epoch = pool->epoch;
    pthread_mutex_unlock(&pool->mutex);

    bench_worker_scan(worker);

    pthread_mutex_lock(&pool->mutex);
    if (pool->active_count > 0 && --pool->active_count == 0)
      pthread_cond_signal(&pool->done_cond);
  }
  pthread_mutex_unlock(&pool->mutex);
  return NULL;
}

static bool bench_pool_init(BenchPool *pool, size_t worker_count,
                            BenchCorpus *corpus,
                            fzf_pattern_t *pattern, bool sort_results,
                            bool tiebreak_length) {
  memset(pool, 0, sizeof *pool);
  if (pthread_mutex_init(&pool->mutex, NULL) != 0) return false;
  if (pthread_cond_init(&pool->start_cond, NULL) != 0) {
    pthread_mutex_destroy(&pool->mutex);
    return false;
  }
  if (pthread_cond_init(&pool->done_cond, NULL) != 0) {
    pthread_cond_destroy(&pool->start_cond);
    pthread_mutex_destroy(&pool->mutex);
    return false;
  }
  pool->workers = calloc(worker_count, sizeof *pool->workers);
  if (!pool->workers) {
    pthread_cond_destroy(&pool->done_cond);
    pthread_cond_destroy(&pool->start_cond);
    pthread_mutex_destroy(&pool->mutex);
    return false;
  }
  pool->worker_count = worker_count;
  pool->corpus = corpus;
  pool->pattern = pattern;
  pool->has_positive_term = pattern->has_positive_term;
  pool->sort_results = sort_results && pattern->has_positive_term;
  pool->tiebreak_length = tiebreak_length;
  atomic_init(&pool->next_chunk, 0);
  atomic_init(&pool->failed, false);

  size_t created = 0;
  for (; created < worker_count; created++) {
    pool->workers[created].pool = pool;
    if (pthread_create(&pool->workers[created].thread, NULL,
                       bench_worker_main, &pool->workers[created]) != 0)
      break;
  }
  if (created != worker_count) {
    pthread_mutex_lock(&pool->mutex);
    pool->stop = true;
    pthread_cond_broadcast(&pool->start_cond);
    pthread_mutex_unlock(&pool->mutex);
    for (size_t i = 0; i < created; i++)
      pthread_join(pool->workers[i].thread, NULL);
    free(pool->workers);
    pthread_cond_destroy(&pool->done_cond);
    pthread_cond_destroy(&pool->start_cond);
    pthread_mutex_destroy(&pool->mutex);
    memset(pool, 0, sizeof *pool);
    return false;
  }

  pthread_mutex_lock(&pool->mutex);
  while (pool->ready_count < pool->worker_count)
    pthread_cond_wait(&pool->done_cond, &pool->mutex);
  pthread_mutex_unlock(&pool->mutex);
  return true;
}

static BenchMerger *bench_make_pass_merger(size_t count) {
  BenchMerger *merger = calloc(1, sizeof *merger);
  if (merger) {
    merger->count = count;
    merger->pass = true;
  }
  return merger;
}

static BenchMerger *bench_make_result_merger(BenchPool *pool,
                                             size_t list_count) {
  BenchMerger *merger = calloc(1, sizeof *merger);
  if (!merger) return NULL;
  merger->list_count = list_count;
  merger->sorted = pool->sort_results;
  if (list_count == 0) return merger;

  merger->lists = calloc(list_count, sizeof *merger->lists);
  merger->cursors = calloc(list_count, sizeof *merger->cursors);
  if (!merger->lists || !merger->cursors) {
    free(merger->cursors);
    free(merger->lists);
    free(merger);
    return NULL;
  }
  for (size_t i = 0; i < list_count; i++) {
    merger->lists[i] = pool->workers[i].matches;
    if (merger->lists[i].count > SIZE_MAX - merger->count) {
      free(merger->cursors);
      free(merger->lists);
      free(merger);
      return NULL;
    }
    merger->count += merger->lists[i].count;
  }
  return merger;
}

static void bench_free_merger(BenchMerger *merger) {
  if (!merger) return;
  free(merger->cursors);
  free(merger->lists);
  free(merger);
}

static void bench_pool_discard_results(BenchPool *pool) {
  if (pool->result.pass_merger != pool->result.merger)
    bench_free_merger(pool->result.pass_merger);
  bench_free_merger(pool->result.merger);
  pool->result = (BenchScanResult){0};
  for (size_t i = 0; i < pool->worker_count; i++) {
    free(pool->workers[i].matches.values);
    pool->workers[i].matches = (BenchMatchList){0};
  }
}

static bool bench_pool_run(BenchPool *pool) {
  /* PassMerger performs this chunk count at the start of matcher.scan. */
  size_t chunk_count = (pool->corpus->count + FZF_BENCH_CHUNK_SIZE - 1) /
      FZF_BENCH_CHUNK_SIZE;
  if (chunk_count == 0) {
    pool->result.merger = bench_make_result_merger(pool, 0);
    return pool->result.merger != NULL;
  }

  size_t pass_count = 0;
  for (size_t first = 0; first < pool->corpus->count;
       first += FZF_BENCH_CHUNK_SIZE) {
    size_t remaining = pool->corpus->count - first;
    pass_count += remaining < FZF_BENCH_CHUNK_SIZE
        ? remaining : FZF_BENCH_CHUNK_SIZE;
  }
  pool->result.pass_merger = bench_make_pass_merger(pass_count);
  if (!pool->result.pass_merger) return false;
  if (pool->pattern->ptr == NULL) {
    pool->result.merger = pool->result.pass_merger;
    return true;
  }

  atomic_store_explicit(&pool->next_chunk, 0, memory_order_relaxed);
  atomic_store_explicit(&pool->failed, false, memory_order_relaxed);
  pthread_mutex_lock(&pool->mutex);
  pool->active_count = pool->worker_count;
  pool->epoch++;
  pthread_cond_broadcast(&pool->start_cond);
  while (pool->active_count > 0)
    pthread_cond_wait(&pool->done_cond, &pool->mutex);
  pthread_mutex_unlock(&pool->mutex);

  if (atomic_load_explicit(&pool->failed, memory_order_relaxed)) return false;
  pool->result.merger = bench_make_result_merger(pool, pool->worker_count);
  return pool->result.merger != NULL;
}

static size_t bench_pool_result_length(const BenchPool *pool) {
  return pool->result.merger ? pool->result.merger->count : 0;
}

static void bench_pool_destroy(BenchPool *pool) {
  pthread_mutex_lock(&pool->mutex);
  pool->stop = true;
  pthread_cond_broadcast(&pool->start_cond);
  pthread_mutex_unlock(&pool->mutex);
  for (size_t i = 0; i < pool->worker_count; i++) {
    pthread_join(pool->workers[i].thread, NULL);
    fzf_free_slab(pool->workers[i].slab);
    free(pool->workers[i].sort_scratch);
  }
  bench_pool_discard_results(pool);
  free(pool->workers);
  pthread_cond_destroy(&pool->done_cond);
  pthread_cond_destroy(&pool->start_cond);
  pthread_mutex_destroy(&pool->mutex);
  memset(pool, 0, sizeof *pool);
}

static uint64_t bench_hash_bytes(uint64_t hash, const void *data,
                                 size_t length) {
  const unsigned char *bytes = data;
  for (size_t i = 0; i < length; i++) {
    hash ^= bytes[i];
    hash *= UINT64_C(1099511628211);
  }
  return hash;
}

static uint64_t bench_hash_u64(uint64_t hash, uint64_t value) {
  unsigned char encoded[8];
  for (unsigned i = 0; i < 8; i++)
    encoded[i] = (unsigned char)(value >> (i * 8));
  return bench_hash_bytes(hash, encoded, sizeof encoded);
}

static uint64_t bench_input_checksum(const BenchCorpus *corpus) {
  static const char version[] = "fzf-native-bench-input-v1";
  uint64_t hash = bench_hash_bytes(
      UINT64_C(14695981039346656037), version, sizeof version - 1);
  hash = bench_hash_u64(hash, corpus->count);
  for (size_t i = 0; i < corpus->count; i++) {
    const BenchCandidate *candidate = &corpus->candidates[i];
    hash = bench_hash_u64(hash, candidate->length);
    hash = bench_hash_bytes(hash, candidate->text, candidate->length);
  }
  return hash;
}

static bool bench_match_precedes(const BenchMatch *left,
                                 const BenchMatch *right,
                                 bool sortable) {
  if (sortable && left->rank_score != right->rank_score)
    return left->rank_score > right->rank_score;
  if (sortable && left->rank_length != right->rank_length)
    return left->rank_length < right->rank_length;
  return left->candidate->index < right->candidate->index;
}

static bool bench_result_checksum(BenchPool *pool, size_t match_count,
                                  uint64_t *checksum) {
  static const char version[] = "fzf-native-bench-result-v1";
  uint64_t hash = bench_hash_bytes(
      UINT64_C(14695981039346656037), version, sizeof version - 1);
  hash = bench_hash_u64(hash, pool->corpus->count);
  hash = bench_hash_u64(hash, match_count);

  if (pool->pattern->ptr == NULL) {
    for (size_t i = 0; i < pool->corpus->count; i++) {
      hash = bench_hash_u64(hash, pool->corpus->candidates[i].index);
      hash = bench_hash_u64(hash, 0);
    }
    *checksum = hash;
    return true;
  }

  BenchMerger *merger = pool->result.merger;
  if (!merger || merger->pass || merger->count != match_count) return false;
  if (merger->list_count > 0)
    memset(merger->cursors, 0,
           merger->list_count * sizeof *merger->cursors);
  for (size_t emitted = 0; emitted < match_count; emitted++) {
    const BenchMatch *best = NULL;
    size_t best_worker = 0;
    for (size_t worker = 0; worker < merger->list_count; worker++) {
      const BenchMatchList *list = &merger->lists[worker];
      if (merger->cursors[worker] >= list->count) continue;
      const BenchMatch *candidate =
          &list->values[merger->cursors[worker]];
      if (!best || bench_match_precedes(
                       candidate, best, merger->sorted)) {
        best = candidate;
        best_worker = worker;
      }
    }
    if (!best) return false;
    hash = bench_hash_u64(hash, best->candidate->index);
    hash = bench_hash_u64(hash, best->rank_score);
    if (pool->tiebreak_length)
      hash = bench_hash_u64(hash, best->rank_length);
    merger->cursors[best_worker]++;
  }
  *checksum = hash;
  return true;
}

static bool bench_write_candidate(const BenchCandidate *candidate) {
  return fwrite(candidate->text, 1, candidate->length, stdout) ==
             candidate->length &&
      fputc('\n', stdout) != EOF;
}

/* Materialize the lazy merge only in this untimed correctness mode.
   The native library preserves malformed bytes.  fzf replaces them with
   U+FFFD during ingestion, so byte-for-byte fzf comparisons require valid
   UTF-8 input. */
static bool bench_dump_results(BenchPool *pool, size_t match_count) {
  if (pool->pattern->ptr == NULL) {
    for (size_t i = 0; i < pool->corpus->count; i++)
      if (!bench_write_candidate(&pool->corpus->candidates[i])) return false;
    return fflush(stdout) == 0;
  }

  BenchMerger *merger = pool->result.merger;
  if (!merger || merger->pass || merger->count != match_count) return false;
  if (merger->list_count > 0)
    memset(merger->cursors, 0,
           merger->list_count * sizeof *merger->cursors);
  bool success = true;
  for (size_t emitted = 0; emitted < match_count; emitted++) {
    const BenchMatch *best = NULL;
    size_t best_worker = 0;
    for (size_t worker = 0; worker < merger->list_count; worker++) {
      const BenchMatchList *list = &merger->lists[worker];
      if (merger->cursors[worker] >= list->count) continue;
      const BenchMatch *candidate =
          &list->values[merger->cursors[worker]];
      if (!best || bench_match_precedes(
                       candidate, best, merger->sorted)) {
        best = candidate;
        best_worker = worker;
      }
    }
    if (!best || !bench_write_candidate(best->candidate)) {
      success = false;
      break;
    }
    merger->cursors[best_worker]++;
  }
  return success && fflush(stdout) == 0;
}

/* Emit the exact identity and rank inputs used by the adapter.  This mode is
   intentionally untimed.  A pinned helper inside the upstream fzf package
   consumes the same candidates for the parity checker. */
static bool bench_dump_semantic(BenchPool *pool, size_t match_count) {
  BenchMerger *merger = pool->result.merger;
  if (!merger || merger->pass || merger->count != match_count) return false;
  if (merger->list_count > 0)
    memset(merger->cursors, 0,
           merger->list_count * sizeof *merger->cursors);

  fzf_slab_t *slab = fzf_make_default_slab();
  if (!slab || !fzf_slab_set_score_scheme(
                   slab, FZF_SCORE_SCHEME_DEFAULT)) {
    fzf_free_slab(slab);
    return false;
  }

  bool success = true;
  for (size_t emitted = 0; emitted < match_count; emitted++) {
    const BenchMatch *best = NULL;
    size_t best_worker = 0;
    for (size_t worker = 0; worker < merger->list_count; worker++) {
      const BenchMatchList *list = &merger->lists[worker];
      if (merger->cursors[worker] >= list->count) continue;
      const BenchMatch *candidate =
          &list->values[merger->cursors[worker]];
      if (!best || bench_match_precedes(candidate, best, merger->sorted)) {
        best = candidate;
        best_worker = worker;
      }
    }
    if (!best) {
      success = false;
      break;
    }

    fzf_score_bounds_t bounds;
    int32_t membership_score = fzf_get_score_with_bounds_bytes_preclassified(
        best->candidate->text, best->candidate->length,
        best->candidate->input_is_ascii, pool->pattern, slab, &bounds);
    uint16_t rank_score = pool->has_positive_term
        ? bench_rank_score(bounds.raw_score) : 0;
    uint16_t rank_length = best->rank_length;
    uint16_t point3 = (uint16_t)(UINT16_MAX - rank_score);
    if (membership_score <= 0 || fzf_allocation_failed() ||
        rank_score != best->rank_score ||
        printf("%" PRIu32 "\t%" PRId64 "\t%d\t%d\t%d\t%d\t"
               "%u\t0\t0\t%u\t%u\n",
               best->candidate->index, bounds.raw_score,
               bounds.min_begin, bounds.min_end, bounds.max_end,
               bounds.valid ? 1 : 0, (unsigned)rank_score,
               (unsigned)rank_length,
               (unsigned)point3) < 0) {
      success = false;
      break;
    }
    merger->cursors[best_worker]++;
  }

  fzf_free_slab(slab);
  return success && fflush(stdout) == 0;
}

static char *bench_duplicate(const char *text) {
  size_t length = strlen(text) + 1;
  char *copy = malloc(length);
  if (copy) memcpy(copy, text, length);
  return copy;
}

int main(int argc, char **argv) {
  BenchOptions options;
  if (!bench_parse_options(argc, argv, &options)) {
    bench_usage(stderr, argv[0]);
    return 2;
  }
  if (options.help) {
    bench_usage(stdout, argv[0]);
    return 0;
  }

  uint64_t ingestion_started = bench_now_ns();
  BenchCorpus corpus;
  if (!bench_load_corpus(stdin, &corpus)) {
    fprintf(stderr, "fzf-native benchmark: could not ingest stdin\n");
    return 1;
  }
  uint64_t ingestion_ns = bench_now_ns() - ingestion_started;
  uint64_t input_checksum = bench_input_checksum(&corpus);

  char *query_copy = bench_duplicate(options.query);
  fzf_pattern_t *pattern = query_copy
      ? fzf_parse_pattern(
            CaseSmart, options.normalize, query_copy, true) : NULL;
  free(query_copy);
  if (!pattern) {
    fprintf(stderr, "fzf-native benchmark: could not parse query\n");
    bench_free_corpus(&corpus);
    return 1;
  }

  size_t chunk_count = (corpus.count + FZF_BENCH_CHUNK_SIZE - 1) /
      FZF_BENCH_CHUNK_SIZE;
  size_t worker_count = options.threads;
  if (chunk_count > 0 && worker_count > chunk_count)
    worker_count = chunk_count;
  if (worker_count == 0) worker_count = 1;

  BenchPool pool;
  if (!bench_pool_init(
          &pool, worker_count, &corpus, pattern, options.sort_results,
          options.tiebreak_length)) {
    fprintf(stderr, "fzf-native benchmark: could not start workers\n");
    fzf_free_pattern(pattern);
    bench_free_corpus(&corpus);
    return 1;
  }

  size_t match_count = 0;
  uint64_t *samples = NULL;
  size_t sample_count = 0;
  size_t sample_capacity = 0;
  bool success = true;
  if (options.check_only || options.dump_results || options.dump_semantic) {
    bench_pool_discard_results(&pool);
    success = bench_pool_run(&pool);
    if (success) match_count = bench_pool_result_length(&pool);
  } else {
    uint64_t started = bench_now_ns();
    uint64_t deadline = UINT64_MAX - started < options.duration_ns
        ? UINT64_MAX : started + options.duration_ns;
    while (bench_now_ns() < deadline) {
      bench_pool_discard_results(&pool);
      uint64_t scan_started = bench_now_ns();
      if (!bench_pool_run(&pool)) {
        success = false;
        break;
      }
      uint64_t elapsed = bench_now_ns() - scan_started;
      match_count = bench_pool_result_length(&pool);
      if (sample_count == sample_capacity) {
        size_t next = sample_capacity ? sample_capacity * 2 : 128;
        if (next < sample_capacity || next > SIZE_MAX / sizeof *samples) {
          success = false;
          break;
        }
        uint64_t *grown = realloc(samples, next * sizeof *grown);
        if (!grown) {
          success = false;
          break;
        }
        samples = grown;
        sample_capacity = next;
      }
      samples[sample_count++] = elapsed;
    }
    if (sample_count == 0) success = false;
  }

  uint64_t total_ns = 0;
  uint64_t minimum = sample_count ? samples[0] : 0;
  uint64_t maximum = sample_count ? samples[0] : 0;
  if (success && !options.check_only && !options.dump_results) {
    for (size_t i = 0; i < sample_count; i++) {
      if (UINT64_MAX - total_ns < samples[i]) {
        success = false;
        break;
      }
      total_ns += samples[i];
      if (samples[i] < minimum) minimum = samples[i];
      if (samples[i] > maximum) maximum = samples[i];
    }
  }

  uint64_t result_checksum = 0;
  if (success && options.dump_results)
    success = bench_dump_results(&pool, match_count);
  else if (success && options.dump_semantic)
    success = bench_dump_semantic(&pool, match_count);
  else if (success)
    success = bench_result_checksum(&pool, match_count, &result_checksum);
  if (!success) {
    fprintf(stderr, "fzf-native benchmark: scan or output failed\n");
  } else if (options.dump_results || options.dump_semantic) {
    /* The selected dump mode has already written its output. */
  } else if (options.check_only) {
    printf("semantic items=%zu matches=%zu input_checksum=%016" PRIx64
           " result_checksum=%016" PRIx64 "\n",
           corpus.count, match_count, input_checksum, result_checksum);
  } else {
    const char *json = getenv("FZF_BENCH_JSON");
    if (json && strcmp(json, "1") == 0) {
      printf("benchmark-json {\"schema\":1,\"iterations\":%zu,"
             "\"total_ns\":%" PRIu64 ",\"min_ns\":%" PRIu64 ","
             "\"max_ns\":%" PRIu64 ",\"items\":%zu,"
             "\"matches\":%zu,\"ingestion_ns\":%" PRIu64 "}\n",
             sample_count, total_ns, minimum, maximum, corpus.count,
             match_count, ingestion_ns);
    } else {
      double average_ms = (double)total_ns / (double)sample_count / 1e6;
      double selectivity = corpus.count
          ? (double)match_count / (double)corpus.count * 100.0 : 0.0;
      printf("  %zu iterations  avg: %.2fms  min: %.2fms  max: %.2fms  "
             "total: %.2fs  items: %zu  matches: %zu (%.2f%%)  "
             "ingestion: %.2fms\n",
             sample_count, average_ms, (double)minimum / 1e6,
             (double)maximum / 1e6, (double)total_ns / 1e9, corpus.count,
             match_count, selectivity, (double)ingestion_ns / 1e6);
    }
    printf("semantic items=%zu matches=%zu input_checksum=%016" PRIx64
           " result_checksum=%016" PRIx64 "\n",
           corpus.count, match_count, input_checksum, result_checksum);
  }

  free(samples);
  bench_pool_destroy(&pool);
  fzf_free_pattern(pattern);
  bench_free_corpus(&corpus);
  return success ? 0 : 1;
}
