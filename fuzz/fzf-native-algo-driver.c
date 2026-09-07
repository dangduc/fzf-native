/* SPDX-License-Identifier: GPL-3.0-or-later
 * Persistent raw-algorithm driver for differential tests.
 *
 * The wire format is shared with fuzz/oracle.  Frames use network byte order
 * and preserve arbitrary pattern and candidate bytes.
 */

#include "fzf.h"

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
  PROTOCOL_VERSION = 1,
  OPCODE_INFO = 0,
  OPCODE_MATCH = 1,
  STATUS_OK = 0,
  STATUS_BAD_REQUEST = 1,
  STATUS_UNSUPPORTED = 2,
  STATUS_INTERNAL = 3,
  FRAME_CAP = 64 * 1024 * 1024,
};

#ifndef FZF_NATIVE_REVISION
#define FZF_NATIVE_REVISION "unknown"
#endif

static uint32_t read_u32(const uint8_t *bytes) {
  return ((uint32_t)bytes[0] << 24) | ((uint32_t)bytes[1] << 16) |
         ((uint32_t)bytes[2] << 8) | (uint32_t)bytes[3];
}

static void write_u32(uint8_t *bytes, uint32_t value) {
  bytes[0] = (uint8_t)(value >> 24);
  bytes[1] = (uint8_t)(value >> 16);
  bytes[2] = (uint8_t)(value >> 8);
  bytes[3] = (uint8_t)value;
}

static void write_i64(uint8_t *bytes, int64_t value) {
  uint64_t encoded = (uint64_t)value;
  for (size_t i = 0; i < 8; i++)
    bytes[i] = (uint8_t)(encoded >> (56 - i * 8));
}

static bool read_exact(void *buffer, size_t size) {
  uint8_t *out = buffer;
  while (size > 0) {
    size_t got = fread(out, 1, size, stdin);
    if (got > 0) {
      out += got;
      size -= got;
      continue;
    }
    if (feof(stdin)) return false;
    if (ferror(stdin) && errno == EINTR) {
      clearerr(stdin);
      continue;
    }
    return false;
  }
  return true;
}

static bool write_exact(const void *buffer, size_t size) {
  const uint8_t *input = buffer;
  while (size > 0) {
    size_t put = fwrite(input, 1, size, stdout);
    if (put > 0) {
      input += put;
      size -= put;
      continue;
    }
    if (ferror(stdout) && errno == EINTR) {
      clearerr(stdout);
      continue;
    }
    return false;
  }
  return fflush(stdout) == 0;
}

static bool send_frame(const uint8_t *payload, size_t size) {
  if (size > UINT32_MAX) return false;
  uint8_t header[4];
  write_u32(header, (uint32_t)size);
  return write_exact(header, sizeof header) && write_exact(payload, size);
}

static bool send_error(uint8_t opcode, uint8_t status, const char *message) {
  size_t message_len = strlen(message);
  if (message_len > UINT32_MAX) return false;
  size_t size = 7 + message_len;
  uint8_t *payload = malloc(size);
  if (!payload) return false;
  payload[0] = PROTOCOL_VERSION;
  payload[1] = opcode;
  payload[2] = status;
  write_u32(payload + 3, (uint32_t)message_len);
  memcpy(payload + 7, message, message_len);
  bool ok = send_frame(payload, size);
  free(payload);
  return ok;
}

static bool send_info(void) {
  const char *revision = FZF_NATIVE_REVISION;
  const char *compiler = __VERSION__;
  size_t revision_len = strlen(revision);
  size_t compiler_len = strlen(compiler);
  if (revision_len > UINT32_MAX || compiler_len > UINT32_MAX) return false;
  size_t size = 3 + 4 + revision_len + 4 + compiler_len;
  uint8_t *payload = malloc(size);
  if (!payload) return false;
  size_t offset = 0;
  payload[offset++] = PROTOCOL_VERSION;
  payload[offset++] = OPCODE_INFO;
  payload[offset++] = STATUS_OK;
  write_u32(payload + offset, (uint32_t)revision_len);
  offset += 4;
  memcpy(payload + offset, revision, revision_len);
  offset += revision_len;
  write_u32(payload + offset, (uint32_t)compiler_len);
  offset += 4;
  memcpy(payload + offset, compiler, compiler_len);
  bool ok = send_frame(payload, size);
  free(payload);
  return ok;
}

static char *lower_pattern(const uint8_t *input, size_t size,
                           size_t *output_size) {
  if (size > (SIZE_MAX - 1) / 4) return NULL;
  size_t capacity = size * 4 + 1;
  char *output = malloc(capacity);
  if (!output) return NULL;

  size_t input_offset = 0;
  size_t output_offset = 0;
  while (input_offset < size) {
    utf8proc_int32_t codepoint;
    utf8proc_ssize_t width = utf8proc_iterate(
        input + input_offset, (utf8proc_ssize_t)(size - input_offset),
        &codepoint);
    if (width <= 0) {
      output[output_offset++] = (char)input[input_offset++];
      continue;
    }
    utf8proc_int32_t folded = utf8proc_case_fold(codepoint);
    utf8proc_ssize_t encoded = utf8proc_encode_char(
        folded, (utf8proc_uint8_t *)output + output_offset);
    if (encoded <= 0) {
      free(output);
      return NULL;
    }
    output_offset += (size_t)encoded;
    input_offset += (size_t)width;
  }
  output[output_offset] = '\0';
  *output_size = output_offset;
  return output;
}

static int compare_u32(const void *left, const void *right) {
  uint32_t a = *(const uint32_t *)left;
  uint32_t b = *(const uint32_t *)right;
  return (a > b) - (a < b);
}

static fzf_algo_t select_algorithm(uint8_t algorithm, bool utf8) {
  switch (algorithm) {
    case 0:
      return utf8 ? fzf_fuzzy_match_v1_utf8 : fzf_fuzzy_match_v1;
    case 1:
      return utf8 ? fzf_fuzzy_match_v2_utf8 : fzf_fuzzy_match_v2;
    case 2:
      return utf8 ? fzf_exact_match_utf8 : fzf_exact_match_naive;
    case 4:
      return utf8 ? fzf_prefix_match_utf8 : fzf_prefix_match;
    case 5:
      return utf8 ? fzf_suffix_match_utf8 : fzf_suffix_match;
    case 6:
      return utf8 ? fzf_equal_match_utf8 : fzf_equal_match;
    default:
      return NULL;
  }
}

static bool send_match_result(uint8_t opcode, const fzf_result_t *result,
                              fzf_position_t *positions) {
  if (positions && positions->size > 1)
    qsort(positions->data, positions->size, sizeof positions->data[0],
          compare_u32);
  size_t count = positions ? positions->size : 0;
  if (count > (FRAME_CAP - 33) / 8) return false;
  size_t size = 33 + count * 8;
  uint8_t *payload = malloc(size);
  if (!payload) return false;

  payload[0] = PROTOCOL_VERSION;
  payload[1] = opcode;
  payload[2] = STATUS_OK;
  payload[3] = result->start >= 0;
  payload[4] = positions != NULL;
  write_i64(payload + 5, result->start);
  write_i64(payload + 13, result->end);
  write_i64(payload + 21, result->score);
  write_u32(payload + 29, (uint32_t)count);
  for (size_t i = 0; i < count; i++)
    write_i64(payload + 33 + i * 8, positions->data[i]);

  bool ok = send_frame(payload, size);
  free(payload);
  return ok;
}

static bool handle_match(const uint8_t *payload, size_t size) {
  if (size < 13)
    return send_error(OPCODE_MATCH, STATUS_BAD_REQUEST,
                      "short match request");
  uint8_t algorithm = payload[2];
  uint8_t scheme = payload[3];
  uint8_t flags = payload[4];
  uint32_t pattern_len = read_u32(payload + 5);
  uint32_t candidate_len = read_u32(payload + 9);
  if ((flags & ~7u) != 0)
    return send_error(OPCODE_MATCH, STATUS_BAD_REQUEST, "invalid flags");
  if ((uint64_t)pattern_len + candidate_len != size - 13)
    return send_error(OPCODE_MATCH, STATUS_BAD_REQUEST, "invalid lengths");
  if (scheme != 0)
    return send_error(OPCODE_MATCH, STATUS_UNSUPPORTED,
                      "fzf-native supports only the default scheme");
  if ((flags & 4u) == 0)
    return send_error(OPCODE_MATCH, STATUS_UNSUPPORTED,
                      "fzf-native supports only forward matching");
  if (algorithm == 3)
    return send_error(OPCODE_MATCH, STATUS_UNSUPPORTED,
                      "fzf-native lacks exact-boundary matching");

  const uint8_t *pattern_bytes = payload + 13;
  const uint8_t *candidate_bytes = pattern_bytes + pattern_len;
  bool case_sensitive = (flags & 1u) != 0;
  bool normalize = (flags & 2u) != 0;
  size_t lowered_len = pattern_len;
  char *owned_pattern = NULL;
  if (case_sensitive) {
    owned_pattern = malloc((size_t)pattern_len + 1);
    if (owned_pattern) {
      memcpy(owned_pattern, pattern_bytes, pattern_len);
      owned_pattern[pattern_len] = '\0';
    }
  } else {
    owned_pattern = lower_pattern(pattern_bytes, pattern_len, &lowered_len);
  }
  char *owned_candidate = malloc((size_t)candidate_len + 1);
  if (!owned_pattern || !owned_candidate) {
    free(owned_candidate);
    free(owned_pattern);
    return send_error(OPCODE_MATCH, STATUS_INTERNAL, "allocation failure");
  }
  memcpy(owned_candidate, candidate_bytes, candidate_len);
  owned_candidate[candidate_len] = '\0';

  bool utf8 = !is_ascii_utf8proc(owned_pattern, lowered_len) ||
              !is_ascii_utf8proc(owned_candidate, candidate_len);
  fzf_algo_t matcher = select_algorithm(algorithm, utf8);
  if (!matcher) {
    free(owned_candidate);
    free(owned_pattern);
    return send_error(OPCODE_MATCH, STATUS_BAD_REQUEST,
                      "invalid algorithm");
  }
  fzf_string_t pattern = {
      .data = owned_pattern,
      .size = lowered_len,
  };
  fzf_string_t candidate = {
      .data = owned_candidate,
      .size = candidate_len,
  };
  fzf_slab_t *slab = fzf_make_default_slab();
  fzf_position_t *positions = fzf_pos_array(0);
  if (!slab || !positions) {
    fzf_free_positions(positions);
    fzf_free_slab(slab);
    free(owned_candidate);
    free(owned_pattern);
    return send_error(OPCODE_MATCH, STATUS_INTERNAL, "allocation failure");
  }

  fzf_clear_allocation_failure();
  fzf_result_t result = matcher(case_sensitive, normalize, &candidate,
                                &pattern, positions, slab);
  bool allocation_failed = fzf_allocation_failed();
  bool ok = allocation_failed
                ? send_error(OPCODE_MATCH, STATUS_INTERNAL,
                             "matcher allocation failure")
                : send_match_result(OPCODE_MATCH, &result, positions);
  fzf_free_positions(positions);
  fzf_free_slab(slab);
  free(owned_candidate);
  free(owned_pattern);
  return ok;
}

static bool handle_frame(const uint8_t *payload, size_t size) {
  if (size < 2)
    return send_error(0xff, STATUS_BAD_REQUEST, "short request");
  if (payload[0] != PROTOCOL_VERSION)
    return send_error(payload[1], STATUS_BAD_REQUEST,
                      "unsupported protocol version");
  if (payload[1] == OPCODE_INFO) {
    if (size != 2)
      return send_error(OPCODE_INFO, STATUS_BAD_REQUEST,
                        "invalid info request");
    return send_info();
  }
  if (payload[1] == OPCODE_MATCH) return handle_match(payload, size);
  return send_error(payload[1], STATUS_BAD_REQUEST, "invalid opcode");
}

int main(void) {
  for (;;) {
    uint8_t header[4];
    int first;
    for (;;) {
      errno = 0;
      first = fgetc(stdin);
      if (first != EOF) break;
      if (feof(stdin)) return 0;
      if (ferror(stdin) && errno == EINTR) {
        clearerr(stdin);
        continue;
      }
      fprintf(stderr, "fzf-native-algo-driver: frame read error\n");
      return 1;
    }
    header[0] = (uint8_t)first;
    if (!read_exact(header + 1, sizeof header - 1)) {
      fprintf(stderr, "fzf-native-algo-driver: truncated frame header\n");
      return 1;
    }
    uint32_t size = read_u32(header);
    if (size > FRAME_CAP) {
      fprintf(stderr, "fzf-native-algo-driver: oversized frame\n");
      return 1;
    }
    uint8_t *payload = malloc(size ? size : 1);
    if (!payload) return 1;
    bool ok = read_exact(payload, size) && handle_frame(payload, size);
    free(payload);
    if (!ok) return 1;
  }
}
