// SPDX-License-Identifier: MIT
#include "fzf.h"

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

// UTF8PROC integration for Unicode support
#include "utf8proc-2.10.0/utf8proc.h"
#include "utf8_char_index.h"

// TODO(conni2461): UNICODE HEADER
#define UNICODE_MAXASCII 0x7f

#define SFREE(x)                                                               \
  if (x) {                                                                     \
    free(x);                                                                   \
  }

/* Helpers */
#define free_alloc(obj)                                                        \
  if ((obj).allocated) {                                                       \
    free((obj).data);                                                          \
  }

#define gen_simple_slice(name, type)                                           \
  typedef struct {                                                             \
    type *data;                                                                \
    size_t size;                                                               \
  } name##_slice_t;                                                            \
  static name##_slice_t slice_##name(type *input, size_t from, size_t to) {    \
    return (name##_slice_t){.data = input + from, .size = to - from};          \
  }

#define gen_slice(name, type)                                                  \
  gen_simple_slice(name, type);                                                \
  static name##_slice_t slice_##name##_right(type *input, size_t to) {         \
    return slice_##name(input, 0, to);                                         \
  }

gen_slice(i16, int16_t);
gen_simple_slice(i32, int32_t);
gen_slice(str, const char);
#undef gen_slice
#undef gen_simple_slice

/* TODO(conni2461): additional types (utf8) */
typedef int32_t char_class;
typedef char byte;

typedef enum {
  ScoreMatch = 16,
  ScoreGapStart = -3,
  ScoreGapExtention = -1,
  BonusBoundary = ScoreMatch / 2,
  BonusNonWord = ScoreMatch / 2,
  BonusCamel123 = BonusBoundary + ScoreGapExtention,
  BonusConsecutive = -(ScoreGapStart + ScoreGapExtention),
  BonusFirstCharMultiplier = 2,
} score_t;

typedef enum {
  CharNonWord = 0,
  CharLower,
  CharUpper,
  CharLetter,
  CharNumber
} char_types;

static int32_t index_byte(fzf_string_t *string, char b) {
  for (size_t i = 0; i < string->size; i++) {
    if (string->data[i] == b) {
      return (int32_t)i;
    }
  }
  return -1;
}

static size_t leading_whitespaces(fzf_string_t *str) {
  size_t whitespaces = 0;
  for (size_t i = 0; i < str->size; i++) {
    if (!isspace((uint8_t)str->data[i])) {
      break;
    }
    whitespaces++;
  }
  return whitespaces;
}

static size_t trailing_whitespaces(fzf_string_t *str) {
  size_t whitespaces = 0;
  /* Count down with a 1-based index so the unsigned `i' never underflows:
     the original `for (size_t i = str->size - 1; i >= 0; i--)' read
     data[SIZE_MAX] on an empty string and looped past 0 on an all-whitespace
     string (out-of-bounds reads). */
  for (size_t i = str->size; i > 0; i--) {
    if (!isspace((uint8_t)str->data[i - 1])) {
      break;
    }
    whitespaces++;
  }
  return whitespaces;
}

static void copy_runes(fzf_string_t *src, fzf_i32_t *destination) {
  for (size_t i = 0; i < src->size; i++) {
    destination->data[i] = (int32_t)src->data[i];
  }
}

static void copy_into_i16(i16_slice_t *src, fzf_i16_t *dest) {
  for (size_t i = 0; i < src->size; i++) {
    dest->data[i] = src->data[i];
  }
}

// char* helpers
static char *trim_whitespace_left(char *str, size_t *len) {
  for (size_t i = 0; i < *len; i++) {
    if (str[0] == ' ') {
      (*len)--;
      str++;
    } else {
      break;
    }
  }
  return str;
}

static bool has_prefix(const char *str, const char *prefix, size_t prefix_len) {
  return strncmp(prefix, str, prefix_len) == 0;
}

static bool has_suffix(const char *str, size_t len, const char *suffix,
                       size_t suffix_len) {
  return len >= suffix_len &&
         strncmp(slice_str(str, len - suffix_len, len).data, suffix,
                 suffix_len) == 0;
}

static char *str_replace_char(char *str, char find, char replace) {
  char *current_pos = strchr(str, find);
  while (current_pos) {
    *current_pos = replace;
    current_pos = strchr(current_pos, find);
  }
  return str;
}

static char *str_replace(char *orig, char *rep, char *with) {
  if (!orig || !rep || !with) {
    return NULL;
  }

  char *result;
  char *ins;
  char *tmp;

  size_t len_rep = strlen(rep);
  size_t len_front = 0;
  size_t len_orig = strlen(orig);
  size_t len_with = strlen(with);
  size_t count = 0;

  if (len_rep == 0) {
    return NULL;
  }

  ins = orig;
  for (; (tmp = strstr(ins, rep)); ++count) {
    ins = tmp + len_rep;
  }

  tmp = result = (char *)malloc(len_orig + (len_with - len_rep) * count + 1);
  if (!result) {
    return NULL;
  }

  while (count--) {
    ins = strstr(orig, rep);
    len_front = (size_t)(ins - orig);
    tmp = strncpy(tmp, orig, len_front) + len_front;
    tmp = strcpy(tmp, with) + len_with;
    orig += len_front + len_rep;
    len_orig -= len_front + len_rep;
  }
  strncpy(tmp, orig, len_orig);
  tmp[len_orig] = 0;
  return result;
}

// UTF-8 aware lowercase conversion
static char *str_tolower(char *str, size_t size) {
  // Check if string is pure ASCII for fast path
  if (is_ascii_utf8proc(str, size)) {
    // Fast ASCII path
    char *lower_str = (char *)malloc((size + 1) * sizeof(char));
    for (size_t i = 0; i < size; i++) {
      lower_str[i] = (char)tolower((uint8_t)str[i]);
    }
    lower_str[size] = '\0';
    return lower_str;
  }
  
  // UTF-8 path: need to handle multibyte characters
  // Allocate worst case (each char could expand to 4 bytes)
  char *lower_str = (char *)malloc((size * 4 + 1) * sizeof(char));
  size_t out_pos = 0;
  size_t in_pos = 0;
  
  while (in_pos < size) {
    utf8proc_int32_t codepoint;
    utf8proc_ssize_t bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(str + in_pos),
      size - in_pos, &codepoint);
    
    if (bytes <= 0) {
      // Invalid UTF-8, copy byte as-is
      lower_str[out_pos++] = str[in_pos++];
      continue;
    }
    
    // Apply case folding
    utf8proc_int32_t folded = utf8proc_case_fold(codepoint);
    
    // Encode back to UTF-8
    utf8proc_ssize_t out_bytes = utf8proc_encode_char(
      folded, (utf8proc_uint8_t*)(lower_str + out_pos));
    
    if (out_bytes > 0) {
      out_pos += out_bytes;
    } else {
      // Encoding failed, copy original
      memcpy(lower_str + out_pos, str + in_pos, bytes);
      out_pos += bytes;
    }
    
    in_pos += bytes;
  }
  
  lower_str[out_pos] = '\0';
  
  // Resize to actual size
  char *result = (char *)realloc(lower_str, out_pos + 1);
  return result ? result : lower_str;
}

static int16_t max16(int16_t a, int16_t b) {
  return (a > b) ? a : b;
}

static size_t min64u(size_t a, size_t b) {
  return (a < b) ? a : b;
}

fzf_position_t *fzf_pos_array(size_t len) {
  fzf_position_t *pos = (fzf_position_t *)malloc(sizeof(fzf_position_t));
  pos->size = 0;
  pos->cap = len;
  if (len > 0) {
    pos->data = (uint32_t *)malloc(len * sizeof(uint32_t));
  } else {
    pos->data = NULL;
  }
  return pos;
}

static void resize_pos(fzf_position_t *pos, size_t add_len, size_t comp) {
  if (!pos) {
    return;
  }
  if (pos->size + comp > pos->cap) {
    pos->cap += add_len > 0 ? add_len : 1;
    pos->data = (uint32_t *)realloc(pos->data, sizeof(uint32_t) * pos->cap);
  }
}

static void unsafe_append_pos(fzf_position_t *pos, size_t value) {
  resize_pos(pos, pos->cap, 1);
  pos->data[pos->size] = value;
  pos->size++;
}

static void append_pos(fzf_position_t *pos, size_t value) {
  if (pos) {
    unsafe_append_pos(pos, value);
  }
}

static void insert_range(fzf_position_t *pos, size_t start, size_t end) {
  if (!pos) {
    return;
  }

  int32_t diff = ((int32_t)end - (int32_t)start);
  if (diff <= 0) {
    return;
  }

  resize_pos(pos, end - start, end - start);
  for (size_t k = start; k < end; k++) {
    pos->data[pos->size] = k;
    pos->size++;
  }
}

static fzf_i16_t alloc16(size_t *offset, fzf_slab_t *slab, size_t size) {
  if (slab != NULL && slab->I16.cap > *offset + size) {
    i16_slice_t slice = slice_i16(slab->I16.data, *offset, (*offset) + size);
    *offset = *offset + size;
    return (fzf_i16_t){.data = slice.data,
                       .size = slice.size,
                       .cap = slice.size,
                       .allocated = false};
  }
  int16_t *data = (int16_t *)malloc(size * sizeof(int16_t));
  memset(data, 0, size * sizeof(int16_t));
  return (fzf_i16_t){
      .data = data, .size = size, .cap = size, .allocated = true};
}

static fzf_i32_t alloc32(size_t *offset, fzf_slab_t *slab, size_t size) {
  if (slab != NULL && slab->I32.cap > *offset + size) {
    i32_slice_t slice = slice_i32(slab->I32.data, *offset, (*offset) + size);
    *offset = *offset + size;
    return (fzf_i32_t){.data = slice.data,
                       .size = slice.size,
                       .cap = slice.size,
                       .allocated = false};
  }
  int32_t *data = (int32_t *)malloc(size * sizeof(int32_t));
  memset(data, 0, size * sizeof(int32_t));
  return (fzf_i32_t){
      .data = data, .size = size, .cap = size, .allocated = true};
}

static char_class char_class_of_ascii(char ch) {
  if (ch >= 'a' && ch <= 'z') {
    return CharLower;
  }
  if (ch >= 'A' && ch <= 'Z') {
    return CharUpper;
  }
  if (ch >= '0' && ch <= '9') {
    return CharNumber;
  }
  return CharNonWord;
}

// static char_class char_class_of_non_ascii(char ch) {
//   return 0;
// }

static char_class char_class_of(char ch) {
  return char_class_of_ascii(ch);
  // if (ch <= 0x7f) {
  //   return char_class_of_ascii(ch);
  // }
  // return char_class_of_non_ascii(ch);
}

static int16_t bonus_for(char_class prev_class, char_class class) {
  if (prev_class == CharNonWord && class != CharNonWord) {
    return BonusBoundary;
  }
  if ((prev_class == CharLower && class == CharUpper) ||
      (prev_class != CharNumber && class == CharNumber)) {
    return BonusCamel123;
  }
  if (class == CharNonWord) {
    return BonusNonWord;
  }
  return 0;
}

static int16_t bonus_at(fzf_string_t *input, size_t idx) {
  if (idx == 0) {
    return BonusBoundary;
  }
  return bonus_for(char_class_of(input->data[idx - 1]),
                   char_class_of(input->data[idx]));
}

/* TODO(conni2461): maybe just not do this */
static char normalize_rune(char r) {
  // TODO(conni2461)
  /* if (r < 0x00C0 || r > 0x2184) { */
  /*   return r; */
  /* } */
  /* rune n = normalized[r]; */
  /* if n > 0 { */
  /*   return n; */
  /* } */
  return r;
}

static int32_t try_skip(fzf_string_t *input, bool case_sensitive, byte b,
                        int32_t from) {
  str_slice_t slice = slice_str(input->data, (size_t)from, input->size);
  fzf_string_t byte_array = {.data = slice.data, .size = slice.size};
  int32_t idx = index_byte(&byte_array, b);
  if (idx == 0) {
    return from;
  }

  if (!case_sensitive && b >= 'a' && b <= 'z') {
    if (idx > 0) {
      str_slice_t tmp = slice_str_right(byte_array.data, (size_t)idx);
      byte_array.data = tmp.data;
      byte_array.size = tmp.size;
    }
    int32_t uidx = index_byte(&byte_array, b - (byte)32);
    if (uidx >= 0) {
      idx = uidx;
    }
  }
  if (idx < 0) {
    return -1;
  }

  return from + idx;
}

static bool is_ascii(const char *runes, size_t size) {
  // TODO(conni2461): future use
  /* for (size_t i = 0; i < size; i++) { */
  /*   if (runes[i] >= 256) { */
  /*     return false; */
  /*   } */
  /* } */
  return true;
}

static int32_t ascii_fuzzy_index(fzf_string_t *input, const char *pattern,
                                 size_t size, bool case_sensitive) {
  if (!is_ascii(pattern, size)) {
    return -1;
  }

  int32_t first_idx = 0;
  int32_t idx = 0;
  for (size_t pidx = 0; pidx < size; pidx++) {
    idx = try_skip(input, case_sensitive, pattern[pidx], idx);
    if (idx < 0) {
      return -1;
    }
    if (pidx == 0 && idx > 0) {
      first_idx = idx - 1;
    }
    idx++;
  }

  return first_idx;
}

/* UTF-8 utility functions using utf8proc */

bool is_ascii_utf8proc(const char *text, size_t len) {
  for (size_t i = 0; i < len; i++) {
    if ((unsigned char)text[i] > 127) {
      return false;
    }
  }
  return true;
}

int32_t char_class_of_utf8proc(utf8proc_int32_t codepoint) {
  const utf8proc_property_t *prop = utf8proc_get_property(codepoint);
  
  switch (prop->category) {
    case UTF8PROC_CATEGORY_LL: // Lowercase letter
      return CharLower;
    case UTF8PROC_CATEGORY_LU: // Uppercase letter  
    case UTF8PROC_CATEGORY_LT: // Titlecase letter
      return CharUpper;
    case UTF8PROC_CATEGORY_LO: // Other letter
    case UTF8PROC_CATEGORY_LM: // Modifier letter
      return CharLetter;
    case UTF8PROC_CATEGORY_ND: // Decimal digit number
    case UTF8PROC_CATEGORY_NL: // Letter number
    case UTF8PROC_CATEGORY_NO: // Other number
      return CharNumber;
    default:
      return CharNonWord;
  }
}

utf8proc_int32_t utf8proc_case_fold(utf8proc_int32_t codepoint) {
  return utf8proc_tolower(codepoint);
}

int32_t utf8_fuzzy_index(fzf_string_t *input, const char *pattern,
                         size_t pattern_len, bool case_sensitive) {
  // Handle empty pattern
  if (pattern_len == 0) {
    return 0;
  }

  // Unified implementation for both ASCII and UTF-8
  // This avoids the broken ascii_fuzzy_index function
  const char *input_ptr = input->data;
  const char *pattern_ptr = pattern;
  
  size_t input_pos = 0;
  size_t pattern_pos = 0;
  int32_t first_idx = -1;  // Initialize to -1 to indicate not found
  int32_t char_idx = 0;
  
  // Process each pattern character
  while (pattern_pos < pattern_len && input_pos < input->size) {
    utf8proc_int32_t pattern_cp, input_cp;
    
    // Decode pattern character (handles both ASCII and UTF-8)
    utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(pattern_ptr + pattern_pos),
      pattern_len - pattern_pos, &pattern_cp);
    if (pattern_bytes < 0) return -1; // Invalid UTF-8
    
    if (!case_sensitive) {
      pattern_cp = utf8proc_case_fold(pattern_cp);
    }
    
    // Search for this pattern character starting from current position
    bool found = false;
    int32_t search_char_idx = char_idx;
    size_t search_pos = input_pos;
    
    while (search_pos < input->size) {
      utf8proc_ssize_t input_bytes = utf8proc_iterate(
        (const utf8proc_uint8_t*)(input_ptr + search_pos),
        input->size - search_pos, &input_cp);
      if (input_bytes < 0) return -1; // Invalid UTF-8
      
      utf8proc_int32_t input_cp_cmp = input_cp;
      if (!case_sensitive) {
        input_cp_cmp = utf8proc_case_fold(input_cp);
      }
      
      if (input_cp_cmp == pattern_cp) {
        // Found the character
        if (pattern_pos == 0) {
          // Store the byte position where the first match starts
          first_idx = search_pos;
        }
        found = true;
        input_pos = search_pos + input_bytes;
        char_idx = search_char_idx + 1;
        break;
      }
      
      search_pos += input_bytes;
      search_char_idx++;
    }
    
    if (!found) {
      return -1; // Pattern character not found
    }
    
    pattern_pos += pattern_bytes;
  }
  
  // If we processed all pattern characters, we have a match
  return (pattern_pos >= pattern_len) ? first_idx : -1;
}

/* UTF-8 helper functions */

// UTF-8 aware character comparison
static bool utf8_char_equal(utf8proc_int32_t cp1, utf8proc_int32_t cp2, 
                           bool case_sensitive, bool normalize) {
  if (normalize) {
    // Apply NFC normalization
    utf8proc_uint8_t buffer1[8], buffer2[8];
    utf8proc_ssize_t len1 = utf8proc_encode_char(cp1, buffer1);
    utf8proc_ssize_t len2 = utf8proc_encode_char(cp2, buffer2);
    
    if (len1 > 0 && len2 > 0) {
      // Null-terminate the buffers for utf8proc_NFC
      buffer1[len1] = 0;
      buffer2[len2] = 0;
      
      utf8proc_uint8_t *norm1 = utf8proc_NFC(buffer1);
      utf8proc_uint8_t *norm2 = utf8proc_NFC(buffer2);
      
      if (norm1 && norm2) {
        utf8proc_int32_t norm_cp1, norm_cp2;
        utf8proc_iterate(norm1, -1, &norm_cp1);
        utf8proc_iterate(norm2, -1, &norm_cp2);
        
        if (!case_sensitive) {
          norm_cp1 = utf8proc_case_fold(norm_cp1);
          norm_cp2 = utf8proc_case_fold(norm_cp2);
        }
        
        free(norm1);
        free(norm2);
        return norm_cp1 == norm_cp2;
      }
      
      if (norm1) free(norm1);
      if (norm2) free(norm2);
    }
  }
  
  // Fallback to simple comparison
  if (!case_sensitive) {
    cp1 = utf8proc_case_fold(cp1);
    cp2 = utf8proc_case_fold(cp2);
  }
  
  return cp1 == cp2;
}

// UTF-8 aware bonus calculation
static int16_t bonus_for_utf8(int32_t prev_class, int32_t class) {
  if (prev_class == CharNonWord && class != CharNonWord) {
    // Word boundary
    return BonusBoundary;
  } else if ((prev_class == CharLower && class == CharUpper) ||
             (prev_class != CharNumber && class == CharNumber)) {
    // camelCase transition or number transition
    return BonusCamel123;
  } else if (class == CharNonWord) {
    return BonusNonWord;
  }
  return 0;
}

static int32_t calculate_score(bool case_sensitive, bool normalize,
                               fzf_string_t *text, fzf_string_t *pattern,
                               size_t sidx, size_t eidx, fzf_position_t *pos) {
  const size_t M = pattern->size;

  size_t pidx = 0;
  int32_t score = 0;
  int32_t consecutive = 0;
  bool in_gap = false;
  int16_t first_bonus = 0;

  resize_pos(pos, M, M);
  int32_t prev_class = CharNonWord;
  if (sidx > 0) {
    prev_class = char_class_of(text->data[sidx - 1]);
  }
  for (size_t idx = sidx; idx < eidx; idx++) {
    char c = text->data[idx];
    int32_t class = char_class_of(c);
    if (!case_sensitive) {
      /* TODO(conni2461): He does some unicode stuff here, investigate */
      c = (char)tolower((uint8_t)c);
    }
    if (normalize) {
      c = normalize_rune(c);
    }
    if (c == pattern->data[pidx]) {
      append_pos(pos, idx);
      score += ScoreMatch;
      int16_t bonus = bonus_for(prev_class, class);
      if (consecutive == 0) {
        first_bonus = bonus;
      } else {
        if (bonus == BonusBoundary) {
          first_bonus = bonus;
        }
        bonus = max16(max16(bonus, first_bonus), BonusConsecutive);
      }
      if (pidx == 0) {
        score += (int32_t)(bonus * BonusFirstCharMultiplier);
      } else {
        score += (int32_t)bonus;
      }
      in_gap = false;
      consecutive++;
      pidx++;
    } else {
      if (in_gap) {
        score += ScoreGapExtention;
      } else {
        score += ScoreGapStart;
      }
      in_gap = true;
      consecutive = 0;
      first_bonus = 0;
    }
    prev_class = class;
  }
  return score;
}

// UTF-8 aware calculate_score
static int32_t calculate_score_utf8(bool case_sensitive, bool normalize,
                                    fzf_string_t *text, fzf_string_t *pattern,
                                    size_t start, size_t end, fzf_position_t *pos) {
  size_t pidx = 0;
  size_t text_pos = start;
  int32_t score = 0;
  int32_t consecutive = 0;
  int16_t first_bonus = 0;
  bool in_gap = false;

  const size_t M = pattern->size;
  const size_t N = text->size;

  // Determine prev_class: the character class of the character before 'start'
  int32_t prev_class = CharNonWord;
  if (start > 0) {
    // Walk forward from 0 to find the codepoint just before start
    size_t scan = 0;
    utf8proc_int32_t prev_cp = 0;
    while (scan < start) {
      utf8proc_int32_t cp;
      utf8proc_ssize_t b = utf8proc_iterate(
        (const utf8proc_uint8_t*)(text->data + scan),
        N - scan, &cp);
      if (b <= 0) break;
      prev_cp = cp;
      scan += b;
    }
    prev_class = char_class_of_utf8proc(prev_cp);
  }

  size_t pat_byte_pos = 0; // tracks current byte offset into pattern

  while (text_pos < end) {
    utf8proc_int32_t text_cp;

    utf8proc_ssize_t text_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(text->data + text_pos),
      end - text_pos, &text_cp);
    if (text_bytes <= 0) break;

    int32_t class = char_class_of_utf8proc(text_cp);
    int16_t bonus = bonus_for_utf8(prev_class, class);

    // Check if we still have pattern characters to match
    if (pat_byte_pos < M) {
      utf8proc_int32_t pattern_cp;
      utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
        (const utf8proc_uint8_t*)(pattern->data + pat_byte_pos),
        M - pat_byte_pos, &pattern_cp);

      if (pattern_bytes > 0 &&
          utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
        append_pos(pos, text_pos);
        score += ScoreMatch;

        if (consecutive == 0) {
          first_bonus = bonus;
        } else {
          if (bonus == BonusBoundary) {
            first_bonus = bonus;
          }
          bonus = max16(max16(bonus, first_bonus), BonusConsecutive);
        }

        if (pidx == 0) {
          score += (int32_t)(bonus * BonusFirstCharMultiplier);
        } else {
          score += (int32_t)bonus;
        }
        in_gap = false;
        consecutive++;
        pidx++;
        pat_byte_pos += pattern_bytes;
      } else {
        if (in_gap) {
          score += ScoreGapExtention;
        } else {
          score += ScoreGapStart;
        }
        in_gap = true;
        consecutive = 0;
        first_bonus = 0;
      }
    }

    text_pos += text_bytes;
    prev_class = class;
  }

  return score;
}

fzf_result_t fzf_fuzzy_match_v1(bool case_sensitive, bool normalize,
                                fzf_string_t *text, fzf_string_t *pattern,
                                fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  const size_t N = text->size;
  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }
  if (ascii_fuzzy_index(text, pattern->data, M, case_sensitive) < 0) {
    return (fzf_result_t){-1, -1, 0};
  }

  int32_t pidx = 0;
  int32_t sidx = -1;
  int32_t eidx = -1;
  for (size_t idx = 0; idx < N; idx++) {
    char c = text->data[idx];
    /* TODO(conni2461): Common pattern maybe a macro would be good here */
    if (!case_sensitive) {
      /* TODO(conni2461): He does some unicode stuff here, investigate */
      c = (char)tolower((uint8_t)c);
    }
    if (normalize) {
      c = normalize_rune(c);
    }
    if (c == pattern->data[pidx]) {
      if (sidx < 0) {
        sidx = (int32_t)idx;
      }
      pidx++;
      if (pidx == M) {
        eidx = (int32_t)idx + 1;
        break;
      }
    }
  }
  if (sidx >= 0 && eidx >= 0) {
    size_t start = (size_t)sidx;
    size_t end = (size_t)eidx;
    pidx--;
    for (size_t idx = end - 1; idx >= start; idx--) {
      char c = text->data[idx];
      if (!case_sensitive) {
        /* TODO(conni2461): He does some unicode stuff here, investigate */
        c = (char)tolower((uint8_t)c);
      }
      if (c == pattern->data[pidx]) {
        pidx--;
        if (pidx < 0) {
          start = idx;
          break;
        }
      }
    }

    int32_t score = calculate_score(case_sensitive, normalize, text, pattern,
                                    start, end, pos);
    return (fzf_result_t){(int32_t)start, (int32_t)end, score};
  }
  return (fzf_result_t){-1, -1, 0};
}

fzf_result_t fzf_fuzzy_match_v2(bool case_sensitive, bool normalize,
                                fzf_string_t *text, fzf_string_t *pattern,
                                fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  const size_t N = text->size;
  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }
  if (slab != NULL && N * M > slab->I16.cap) {
    return fzf_fuzzy_match_v1(case_sensitive, normalize, text, pattern, pos,
                              slab);
  }

  size_t idx;
  {
    int32_t tmp_idx = ascii_fuzzy_index(text, pattern->data, M, case_sensitive);
    if (tmp_idx < 0) {
      return (fzf_result_t){-1, -1, 0};
    }
    idx = (size_t)tmp_idx;
  }

  size_t offset16 = 0;
  size_t offset32 = 0;

  fzf_i16_t h0 = alloc16(&offset16, slab, N);
  fzf_i16_t c0 = alloc16(&offset16, slab, N);
  // Bonus point for each positions
  fzf_i16_t bo = alloc16(&offset16, slab, N);
  // The first occurrence of each character in the pattern
  fzf_i32_t f = alloc32(&offset32, slab, M);
  // Rune array
  fzf_i32_t t = alloc32(&offset32, slab, N);
  copy_runes(text, &t); // input.CopyRunes(T)

  // Phase 2. Calculate bonus for each point
  int16_t max_score = 0;
  size_t max_score_pos = 0;

  size_t pidx = 0;
  size_t last_idx = 0;

  char pchar0 = pattern->data[0];
  char pchar = pattern->data[0];
  int16_t prev_h0 = 0;
  int32_t prev_class = CharNonWord;
  bool in_gap = false;

  i32_slice_t t_sub = slice_i32(t.data, idx, t.size); // T[idx:];
  i16_slice_t h0_sub =
      slice_i16_right(slice_i16(h0.data, idx, h0.size).data, t_sub.size);
  i16_slice_t c0_sub =
      slice_i16_right(slice_i16(c0.data, idx, c0.size).data, t_sub.size);
  i16_slice_t b_sub =
      slice_i16_right(slice_i16(bo.data, idx, bo.size).data, t_sub.size);

  for (size_t off = 0; off < t_sub.size; off++) {
    char_class class;
    char c = (char)t_sub.data[off];
    class = char_class_of_ascii(c);
    if (!case_sensitive && class == CharUpper) {
      /* TODO(conni2461): unicode support */
      c = (char)tolower((uint8_t)c);
    }
    if (normalize) {
      c = normalize_rune(c);
    }

    t_sub.data[off] = (uint8_t)c;
    int16_t bonus = bonus_for(prev_class, class);
    b_sub.data[off] = bonus;
    prev_class = class;
    if (c == pchar) {
      if (pidx < M) {
        f.data[pidx] = (int32_t)(idx + off);
        pidx++;
        pchar = pattern->data[min64u(pidx, M - 1)];
      }
      last_idx = idx + off;
    }

    if (c == pchar0) {
      int16_t score = ScoreMatch + bonus * BonusFirstCharMultiplier;
      h0_sub.data[off] = score;
      c0_sub.data[off] = 1;
      if (M == 1 && (score > max_score)) {
        max_score = score;
        max_score_pos = idx + off;
        if (bonus == BonusBoundary) {
          break;
        }
      }
      in_gap = false;
    } else {
      if (in_gap) {
        h0_sub.data[off] = max16(prev_h0 + ScoreGapExtention, 0);
      } else {
        h0_sub.data[off] = max16(prev_h0 + ScoreGapStart, 0);
      }
      c0_sub.data[off] = 0;
      in_gap = true;
    }
    prev_h0 = h0_sub.data[off];
  }
  if (pidx != M) {
    free_alloc(t);
    free_alloc(f);
    free_alloc(bo);
    free_alloc(c0);
    free_alloc(h0);
    return (fzf_result_t){-1, -1, 0};
  }
  if (M == 1) {
    free_alloc(t);
    free_alloc(f);
    free_alloc(bo);
    free_alloc(c0);
    free_alloc(h0);
    fzf_result_t res = {(int32_t)max_score_pos, (int32_t)max_score_pos + 1,
                        max_score};
    append_pos(pos, max_score_pos);
    return res;
  }

  size_t f0 = (size_t)f.data[0];
  size_t width = last_idx - f0 + 1;
  fzf_i16_t h = alloc16(&offset16, slab, width * M);
  {
    i16_slice_t h0_tmp_slice = slice_i16(h0.data, f0, last_idx + 1);
    copy_into_i16(&h0_tmp_slice, &h);
  }

  fzf_i16_t c = alloc16(&offset16, slab, width * M);
  {
    i16_slice_t c0_tmp_slice = slice_i16(c0.data, f0, last_idx + 1);
    copy_into_i16(&c0_tmp_slice, &c);
  }

  i32_slice_t f_sub = slice_i32(f.data, 1, f.size);
  str_slice_t p_sub =
      slice_str_right(slice_str(pattern->data, 1, M).data, f_sub.size);
  for (size_t off = 0; off < f_sub.size; off++) {
    size_t foff = (size_t)f_sub.data[off];
    pchar = p_sub.data[off];
    pidx = off + 1;
    size_t row = pidx * width;
    in_gap = false;
    t_sub = slice_i32(t.data, foff, last_idx + 1);
    b_sub = slice_i16_right(slice_i16(bo.data, foff, bo.size).data, t_sub.size);
    i16_slice_t c_sub = slice_i16_right(
        slice_i16(c.data, row + foff - f0, c.size).data, t_sub.size);
    i16_slice_t c_diag = slice_i16_right(
        slice_i16(c.data, row + foff - f0 - 1 - width, c.size).data,
        t_sub.size);
    i16_slice_t h_sub = slice_i16_right(
        slice_i16(h.data, row + foff - f0, h.size).data, t_sub.size);
    i16_slice_t h_diag = slice_i16_right(
        slice_i16(h.data, row + foff - f0 - 1 - width, h.size).data,
        t_sub.size);
    i16_slice_t h_left = slice_i16_right(
        slice_i16(h.data, row + foff - f0 - 1, h.size).data, t_sub.size);
    h_left.data[0] = 0;
    for (size_t j = 0; j < t_sub.size; j++) {
      char ch = (char)t_sub.data[j];
      size_t col = j + foff;
      int16_t s1 = 0;
      int16_t s2 = 0;
      int16_t consecutive = 0;

      if (in_gap) {
        s2 = h_left.data[j] + ScoreGapExtention;
      } else {
        s2 = h_left.data[j] + ScoreGapStart;
      }

      if (pchar == ch) {
        s1 = h_diag.data[j] + ScoreMatch;
        int16_t b = b_sub.data[j];
        consecutive = c_diag.data[j] + 1;
        if (b == BonusBoundary) {
          consecutive = 1;
        } else if (consecutive > 1) {
          b = max16(b, max16(BonusConsecutive,
                             bo.data[col - ((size_t)consecutive) + 1]));
        }
        if (s1 + b < s2) {
          s1 += b_sub.data[j];
          consecutive = 0;
        } else {
          s1 += b;
        }
      }
      c_sub.data[j] = consecutive;
      in_gap = s1 < s2;
      int16_t score = max16(max16(s1, s2), 0);
      if (pidx == M - 1 && (score > max_score)) {
        max_score = score;
        max_score_pos = col;
      }
      h_sub.data[j] = score;
    }
  }

  resize_pos(pos, M, M);
  size_t j = max_score_pos;
  if (pos) {
    size_t i = M - 1;
    bool prefer_match = true;
    for (;;) {
      size_t ii = i * width;
      size_t j0 = j - f0;
      int16_t s = h.data[ii + j0];

      int16_t s1 = 0;
      int16_t s2 = 0;
      if (i > 0 && j >= f.data[i]) {
        s1 = h.data[ii - width + j0 - 1];
      }
      if (j > f.data[i]) {
        s2 = h.data[ii + j0 - 1];
      }

      if (s > s1 && (s > s2 || (s == s2 && prefer_match))) {
        unsafe_append_pos(pos, j);
        if (i == 0) {
          break;
        }
        i--;
      }
      prefer_match = c.data[ii + j0] > 1 || (ii + width + j0 + 1 < c.size &&
                                             c.data[ii + width + j0 + 1] > 0);
      j--;
    }
  }

  free_alloc(h);
  free_alloc(c);
  free_alloc(t);
  free_alloc(f);
  free_alloc(bo);
  free_alloc(c0);
  free_alloc(h0);
  return (fzf_result_t){(int32_t)j, (int32_t)max_score_pos + 1,
                        (int32_t)max_score};
}

fzf_result_t fzf_exact_match_naive(bool case_sensitive, bool normalize,
                                   fzf_string_t *text, fzf_string_t *pattern,
                                   fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  const size_t N = text->size;

  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }
  if (N < M) {
    return (fzf_result_t){-1, -1, 0};
  }
  if (ascii_fuzzy_index(text, pattern->data, M, case_sensitive) < 0) {
    return (fzf_result_t){-1, -1, 0};
  }

  size_t pidx = 0;
  int32_t best_pos = -1;
  int16_t bonus = 0;
  int16_t best_bonus = -1;
  for (size_t idx = 0; idx < N; idx++) {
    char c = text->data[idx];
    if (!case_sensitive) {
      /* TODO(conni2461): He does some unicode stuff here, investigate */
      c = (char)tolower((uint8_t)c);
    }
    if (normalize) {
      c = normalize_rune(c);
    }
    if (c == pattern->data[pidx]) {
      if (pidx == 0) {
        bonus = bonus_at(text, idx);
      }
      pidx++;
      if (pidx == M) {
        if (bonus > best_bonus) {
          best_pos = (int32_t)idx;
          best_bonus = bonus;
        }
        if (bonus == BonusBoundary) {
          break;
        }
        idx -= pidx - 1;
        pidx = 0;
        bonus = 0;
      }
    } else {
      idx -= pidx;
      pidx = 0;
      bonus = 0;
    }
  }
  if (best_pos >= 0) {
    size_t bp = (size_t)best_pos;
    size_t sidx = bp - M + 1;
    size_t eidx = bp + 1;
    int32_t score = calculate_score(case_sensitive, normalize, text, pattern,
                                    sidx, eidx, NULL);
    insert_range(pos, sidx, eidx);
    return (fzf_result_t){(int32_t)sidx, (int32_t)eidx, score};
  }
  return (fzf_result_t){-1, -1, 0};
}

fzf_result_t fzf_prefix_match(bool case_sensitive, bool normalize,
                              fzf_string_t *text, fzf_string_t *pattern,
                              fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }
  size_t trimmed_len = 0;
  /* TODO(conni2461): i feel this is wrong */
  if (!isspace((uint8_t)pattern->data[0])) {
    trimmed_len = leading_whitespaces(text);
  }
  if (text->size - trimmed_len < M) {
    return (fzf_result_t){-1, -1, 0};
  }
  for (size_t i = 0; i < M; i++) {
    char c = text->data[trimmed_len + i];
    if (!case_sensitive) {
      c = (char)tolower((uint8_t)c);
    }
    if (normalize) {
      c = normalize_rune(c);
    }
    if (c != pattern->data[i]) {
      return (fzf_result_t){-1, -1, 0};
    }
  }
  size_t start = trimmed_len;
  size_t end = trimmed_len + M;
  int32_t score = calculate_score(case_sensitive, normalize, text, pattern,
                                  start, end, NULL);
  insert_range(pos, start, end);
  return (fzf_result_t){(int32_t)start, (int32_t)end, score};
}

fzf_result_t fzf_suffix_match(bool case_sensitive, bool normalize,
                              fzf_string_t *text, fzf_string_t *pattern,
                              fzf_position_t *pos, fzf_slab_t *slab) {
  size_t trimmed_len = text->size;
  const size_t M = pattern->size;
  /* TODO(conni2461): i think this is wrong */
  if (M == 0 || !isspace((uint8_t)pattern->data[M - 1])) {
    trimmed_len -= trailing_whitespaces(text);
  }
  if (M == 0) {
    return (fzf_result_t){(int32_t)trimmed_len, (int32_t)trimmed_len, 0};
  }
  /* `trimmed_len' and `M' are unsigned, so the original `size_t diff =
     trimmed_len - M; if (diff < 0)' guard was dead and underflowed when the
     candidate was shorter than the suffix pattern, indexing far out of
     bounds below.  Compare before subtracting. (The _utf8 variant already
     guards with `trimmed_len < M'.) */
  if (trimmed_len < M) {
    return (fzf_result_t){-1, -1, 0};
  }
  size_t diff = trimmed_len - M;

  for (size_t idx = 0; idx < M; idx++) {
    char c = text->data[idx + diff];
    if (!case_sensitive) {
      c = (char)tolower((uint8_t)c);
    }
    if (normalize) {
      c = normalize_rune(c);
    }
    if (c != pattern->data[idx]) {
      return (fzf_result_t){-1, -1, 0};
    }
  }
  size_t start = trimmed_len - M;
  size_t end = trimmed_len;
  int32_t score = calculate_score(case_sensitive, normalize, text, pattern,
                                  start, end, NULL);
  insert_range(pos, start, end);
  return (fzf_result_t){(int32_t)start, (int32_t)end, score};
}

fzf_result_t fzf_equal_match(bool case_sensitive, bool normalize,
                             fzf_string_t *text, fzf_string_t *pattern,
                             fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  if (M == 0) {
    return (fzf_result_t){-1, -1, 0};
  }

  size_t trimmed_len = leading_whitespaces(text);
  size_t trimmed_end_len = trailing_whitespaces(text);

  if ((text->size - trimmed_len - trimmed_end_len) != M) {
    return (fzf_result_t){-1, -1, 0};
  }

  bool match = true;
  if (normalize) {
    // TODO(conni2461): to rune
    for (size_t idx = 0; idx < M; idx++) {
      char pchar = pattern->data[idx];
      char c = text->data[trimmed_len + idx];
      if (!case_sensitive) {
        c = (char)tolower((uint8_t)c);
      }
      if (normalize_rune(c) != normalize_rune(pchar)) {
        match = false;
        break;
      }
    }
  } else {
    // TODO(conni2461): to rune
    for (size_t idx = 0; idx < M; idx++) {
      char pchar = pattern->data[idx];
      char c = text->data[trimmed_len + idx];
      if (!case_sensitive) {
        c = (char)tolower((uint8_t)c);
      }
      if (c != pchar) {
        match = false;
        break;
      }
    }
  }
  if (match) {
    insert_range(pos, trimmed_len, trimmed_len + M);
    return (fzf_result_t){(int32_t)trimmed_len,
                          ((int32_t)trimmed_len + (int32_t)M),
                          (ScoreMatch + BonusBoundary) * (int32_t)M +
                              (BonusFirstCharMultiplier - 1) * BonusBoundary};
  }
  return (fzf_result_t){-1, -1, 0};
}

/* UTF-8 aware matching algorithms */

fzf_result_t fzf_exact_match_utf8(bool case_sensitive, bool normalize,
                                  fzf_string_t *text, fzf_string_t *pattern,
                                  fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  const size_t N = text->size;

  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }
  if (N < M) {
    return (fzf_result_t){-1, -1, 0};
  }
  if (utf8_fuzzy_index(text, pattern->data, M, case_sensitive) < 0) {
    return (fzf_result_t){-1, -1, 0};
  }
  
  // Build byte-to-char mapping for position conversion
  utf8_char_map_t *char_map = utf8_build_char_map(text->data, N);

  // Count pattern codepoints
  size_t pattern_cp_count = 0;
  {
    size_t p = 0;
    while (p < M) {
      utf8proc_int32_t cp;
      utf8proc_ssize_t b = utf8proc_iterate(
        (const utf8proc_uint8_t*)(pattern->data + p), M - p, &cp);
      if (b <= 0) break;
      p += b;
      pattern_cp_count++;
    }
  }

  // UTF-8 exact matching
  size_t text_pos = 0;
  size_t pattern_pos = 0;
  size_t pidx = 0;
  int32_t best_pos = -1;
  int16_t bonus = 0;
  int16_t best_bonus = -1;
  size_t match_start_byte = 0;
  size_t match_start_first_char_bytes = 0;

  while (text_pos < N) {
    utf8proc_int32_t text_cp, pattern_cp;

    // Decode text character
    utf8proc_ssize_t text_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(text->data + text_pos),
      N - text_pos, &text_cp);
    if (text_bytes <= 0) break;

    // Decode pattern character
    utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(pattern->data + pattern_pos),
      M - pattern_pos, &pattern_cp);
    if (pattern_bytes <= 0) break;

    if (utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
      if (pidx == 0) {
        match_start_byte = text_pos;
        match_start_first_char_bytes = (size_t)text_bytes;
        // Use UTF-8 aware bonus for the match start
        int32_t cur_class = char_class_of_utf8proc(text_cp);
        if (text_pos == 0) {
          bonus = BonusBoundary;
        } else {
          // Get previous character's class
          utf8proc_int32_t prev_cp;
          // Walk forward from start to find the character just before text_pos
          size_t prev_pos = 0;
          size_t last_prev_pos = 0;
          utf8proc_int32_t last_prev_cp = 0;
          while (prev_pos < text_pos) {
            utf8proc_ssize_t pb = utf8proc_iterate(
              (const utf8proc_uint8_t*)(text->data + prev_pos),
              text_pos - prev_pos, &prev_cp);
            if (pb <= 0) break;
            last_prev_pos = prev_pos;
            last_prev_cp = prev_cp;
            prev_pos += pb;
          }
          int32_t prev_class = char_class_of_utf8proc(last_prev_cp);
          bonus = bonus_for_utf8(prev_class, cur_class);
        }
      }
      pidx++;
      pattern_pos += pattern_bytes;
      text_pos += text_bytes;

      if (pidx == pattern_cp_count) { // All pattern characters matched
        if (bonus > best_bonus) {
          best_pos = (int32_t)match_start_byte;
          best_bonus = bonus;
        }
        if (bonus == BonusBoundary) {
          break;
        }
        // Reset for next potential match — advance by first matched char's byte length
        text_pos = match_start_byte + match_start_first_char_bytes;
        pattern_pos = 0;
        pidx = 0;
        bonus = 0;
        continue;
      }
    } else {
      if (pidx > 0) {
        // Reset pattern matching — advance by first matched char's byte length
        text_pos = match_start_byte + match_start_first_char_bytes;
        pattern_pos = 0;
        pidx = 0;
        bonus = 0;
        continue;
      }
      text_pos += text_bytes;
    }
  }
  
  if (best_pos >= 0) {
    size_t sidx = (size_t)best_pos;

    // Walk through pattern characters from sidx to find the actual end byte position
    size_t eidx = sidx;
    pattern_pos = 0;
    while (pattern_pos < M && eidx < N) {
      utf8proc_int32_t text_cp_tmp, pattern_cp_tmp;

      utf8proc_ssize_t tb = utf8proc_iterate(
        (const utf8proc_uint8_t*)(text->data + eidx),
        N - eidx, &text_cp_tmp);
      utf8proc_ssize_t pb = utf8proc_iterate(
        (const utf8proc_uint8_t*)(pattern->data + pattern_pos),
        M - pattern_pos, &pattern_cp_tmp);

      if (tb <= 0 || pb <= 0) break;

      eidx += tb;
      pattern_pos += pb;
    }

    int32_t score = calculate_score_utf8(case_sensitive, normalize, text, pattern,
                                         sidx, eidx, NULL);

    // Convert byte positions to character positions
    size_t char_start = utf8_byte_to_char(char_map, sidx);
    size_t char_end = utf8_byte_to_char(char_map, eidx);

    insert_range(pos, char_start, char_end);

    utf8_free_char_map(char_map);
    return (fzf_result_t){(int32_t)char_start, (int32_t)char_end, score};
  }
  utf8_free_char_map(char_map);
  return (fzf_result_t){-1, -1, 0};
}

fzf_result_t fzf_prefix_match_utf8(bool case_sensitive, bool normalize,
                                   fzf_string_t *text, fzf_string_t *pattern,
                                   fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }

  // Build byte-to-char mapping for position conversion
  utf8_char_map_t *char_map = utf8_build_char_map(text->data, text->size);

  // Skip leading whitespace if pattern doesn't start with whitespace
  size_t trimmed_start = 0;
  if (M > 0) {
    utf8proc_int32_t first_pattern_cp;
    utf8proc_iterate((const utf8proc_uint8_t*)pattern->data, M, &first_pattern_cp);
    if (utf8proc_get_property(first_pattern_cp)->category != UTF8PROC_CATEGORY_ZS) {
      trimmed_start = leading_whitespaces(text);
    }
  }
  
  if (text->size - trimmed_start < M) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }

  // Match UTF-8 characters at the beginning
  size_t text_pos = trimmed_start;
  size_t pattern_pos = 0;
  
  while (pattern_pos < M && text_pos < text->size) {
    utf8proc_int32_t text_cp, pattern_cp;
    
    utf8proc_ssize_t text_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(text->data + text_pos),
      text->size - text_pos, &text_cp);
    utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(pattern->data + pattern_pos),
      M - pattern_pos, &pattern_cp);
      
    if (text_bytes <= 0 || pattern_bytes <= 0) {
      utf8_free_char_map(char_map);
      return (fzf_result_t){-1, -1, 0};
    }
    
    if (!utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
      utf8_free_char_map(char_map);
      return (fzf_result_t){-1, -1, 0};
    }
    
    text_pos += text_bytes;
    pattern_pos += pattern_bytes;
  }
  
  if (pattern_pos < M) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }
  
  size_t start = trimmed_start;
  size_t end = text_pos;
  int32_t score = calculate_score_utf8(case_sensitive, normalize, text, pattern,
                                       start, end, NULL);

  // Convert byte positions to character positions
  size_t char_start = utf8_byte_to_char(char_map, start);
  size_t char_end = utf8_byte_to_char(char_map, end);

  // Insert character positions instead of byte positions
  insert_range(pos, char_start, char_end);

  utf8_free_char_map(char_map);
  return (fzf_result_t){(int32_t)char_start, (int32_t)char_end, score};
}

fzf_result_t fzf_suffix_match_utf8(bool case_sensitive, bool normalize,
                                   fzf_string_t *text, fzf_string_t *pattern,
                                   fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  size_t trimmed_len = text->size;
  
  // Build byte-to-char mapping for position conversion
  utf8_char_map_t *char_map = utf8_build_char_map(text->data, text->size);
  
  // Skip trailing whitespace if pattern doesn't end with whitespace
  if (M > 0) {
    utf8proc_int32_t last_pattern_cp = 0;
    // Forward scan to find the last codepoint in the pattern
    size_t scan_pos = 0;
    while (scan_pos < M) {
      utf8proc_int32_t cp;
      utf8proc_ssize_t bytes = utf8proc_iterate(
        (const utf8proc_uint8_t*)(pattern->data + scan_pos),
        M - scan_pos, &cp);
      if (bytes <= 0) break;
      last_pattern_cp = cp;
      scan_pos += bytes;
    }

    if (scan_pos > 0 &&
        utf8proc_get_property(last_pattern_cp)->category != UTF8PROC_CATEGORY_ZS) {
      trimmed_len -= trailing_whitespaces(text);
    }
  }
  
  if (M == 0) {
    size_t char_len = utf8_byte_to_char(char_map, trimmed_len);
    utf8_free_char_map(char_map);
    return (fzf_result_t){(int32_t)char_len, (int32_t)char_len, 0};
  }
  
  if (trimmed_len < M) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }

  // Simpler approach: scan from start to find the pattern at the end
  size_t text_pos = 0;
  size_t best_match_start = 0;
  bool found_match = false;
  
  // Find all occurrences and keep the last one
  while (text_pos <= trimmed_len - M) {
    size_t temp_text_pos = text_pos;
    size_t pattern_pos = 0;
    bool match = true;
    size_t start_pos = temp_text_pos;
    
    // Try to match the pattern at this position
    while (pattern_pos < M && temp_text_pos < trimmed_len) {
      utf8proc_int32_t text_cp, pattern_cp;
      
      utf8proc_ssize_t text_bytes = utf8proc_iterate(
        (const utf8proc_uint8_t*)(text->data + temp_text_pos),
        trimmed_len - temp_text_pos, &text_cp);
      utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
        (const utf8proc_uint8_t*)(pattern->data + pattern_pos),
        M - pattern_pos, &pattern_cp);
        
      if (text_bytes <= 0 || pattern_bytes <= 0) {
        match = false;
        break;
      }
      
      if (!utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
        match = false;
        break;
      }
      
      temp_text_pos += text_bytes;
      pattern_pos += pattern_bytes;
    }
    
    // Check if we matched the full pattern and it ends at trimmed_len
    if (match && pattern_pos >= M && temp_text_pos == trimmed_len) {
      found_match = true;
      best_match_start = start_pos;
      break; // This is a suffix match
    }
    
    // Move to next byte position for next attempt
    utf8proc_int32_t dummy;
    utf8proc_ssize_t skip_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(text->data + text_pos),
      trimmed_len - text_pos, &dummy);
    text_pos += (skip_bytes > 0) ? skip_bytes : 1;
  }
  
  if (!found_match) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }
  
  size_t start = best_match_start;
  size_t end = trimmed_len;
  int32_t score = calculate_score_utf8(case_sensitive, normalize, text, pattern,
                                       start, end, NULL);

  // Convert byte positions to character positions
  size_t char_start = utf8_byte_to_char(char_map, start);
  size_t char_end = utf8_byte_to_char(char_map, end);

  // Insert character positions instead of byte positions
  insert_range(pos, char_start, char_end);

  utf8_free_char_map(char_map);
  return (fzf_result_t){(int32_t)char_start, (int32_t)char_end, score};
}

fzf_result_t fzf_equal_match_utf8(bool case_sensitive, bool normalize,
                                  fzf_string_t *text, fzf_string_t *pattern,
                                  fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  if (M == 0) {
    return (fzf_result_t){-1, -1, 0};
  }

  // Build byte-to-char mapping for position conversion
  utf8_char_map_t *char_map = utf8_build_char_map(text->data, text->size);

  size_t trimmed_start = leading_whitespaces(text);
  size_t trimmed_end_len = trailing_whitespaces(text);
  size_t content_byte_len = text->size - trimmed_start - trimmed_end_len;

  // Count codepoints in text content and pattern for accurate comparison
  size_t text_cp_count = utf8_strlen(text->data + trimmed_start, content_byte_len);
  size_t pattern_cp_count = utf8_strlen(pattern->data, M);

  if (text_cp_count != pattern_cp_count) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }

  // Character-by-character comparison
  size_t text_pos = trimmed_start;
  size_t pattern_pos = 0;
  size_t char_count = 0;
  
  size_t content_end = trimmed_start + content_byte_len;
  while (pattern_pos < M && text_pos < content_end) {
    utf8proc_int32_t text_cp, pattern_cp;

    utf8proc_ssize_t text_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(text->data + text_pos),
      content_end - text_pos, &text_cp);
    utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(pattern->data + pattern_pos),
      M - pattern_pos, &pattern_cp);

    if (text_bytes <= 0 || pattern_bytes <= 0) {
      utf8_free_char_map(char_map);
      return (fzf_result_t){-1, -1, 0};
    }

    if (!utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
      utf8_free_char_map(char_map);
      return (fzf_result_t){-1, -1, 0};
    }

    text_pos += text_bytes;
    pattern_pos += pattern_bytes;
    char_count++;
  }

  // Ensure we consumed all of both strings
  if (pattern_pos != M || text_pos != content_end) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }

  // Convert byte positions to character positions
  size_t char_start = utf8_byte_to_char(char_map, trimmed_start);
  size_t char_end = utf8_byte_to_char(char_map, content_end);
  
  // Insert character positions instead of byte positions
  insert_range(pos, char_start, char_end);
  
  utf8_free_char_map(char_map);
  return (fzf_result_t){(int32_t)char_start,
                        (int32_t)char_end,
                        (ScoreMatch + BonusBoundary) * (int32_t)char_count +
                            (BonusFirstCharMultiplier - 1) * BonusBoundary};
}

fzf_result_t fzf_fuzzy_match_v1_utf8(bool case_sensitive, bool normalize,
                                     fzf_string_t *text, fzf_string_t *pattern,
                                     fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  const size_t N = text->size;
  
  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }
  
  // Check if pattern exists in text using UTF-8 aware fuzzy index
  if (utf8_fuzzy_index(text, pattern->data, M, case_sensitive) < 0) {
    return (fzf_result_t){-1, -1, 0};
  }
  
  // Build byte-to-char mapping for position conversion
  utf8_char_map_t *char_map = utf8_build_char_map(text->data, N);
  
  // Forward scan to find the first occurrence of all pattern characters
  size_t pidx = 0;
  int32_t sidx = -1;
  int32_t eidx = -1;
  size_t text_pos = 0;
  size_t pattern_pos = 0;
  
  while (text_pos < N && pattern_pos < M) {
    utf8proc_int32_t text_cp, pattern_cp;
    
    // Decode text character
    utf8proc_ssize_t text_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(text->data + text_pos),
      N - text_pos, &text_cp);
    if (text_bytes <= 0) break;
    
    // Decode pattern character
    utf8proc_ssize_t pattern_bytes = utf8proc_iterate(
      (const utf8proc_uint8_t*)(pattern->data + pattern_pos),
      M - pattern_pos, &pattern_cp);
    if (pattern_bytes <= 0) break;
    
    // Check if characters match
    if (utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
      if (sidx < 0) {
        sidx = (int32_t)text_pos;
      }
      pattern_pos += pattern_bytes;
      pidx++;
      if (pattern_pos >= M) {
        eidx = (int32_t)(text_pos + text_bytes);
        break;
      }
    }
    
    text_pos += text_bytes;
  }
  
  if (sidx >= 0 && eidx >= 0) {
    // Backward scan to tighten the range
    size_t start = (size_t)sidx;
    size_t end = (size_t)eidx;
    
    // Count pattern characters backwards
    pidx = 0;
    pattern_pos = M;
    while (pattern_pos > 0 && pidx < M) {
      // Move back one UTF-8 character in pattern
      size_t temp_pos = 0;
      size_t last_pos = 0;
      while (temp_pos < pattern_pos) {
        utf8proc_int32_t temp_cp;
        utf8proc_ssize_t temp_bytes = utf8proc_iterate(
          (const utf8proc_uint8_t*)(pattern->data + temp_pos),
          pattern_pos - temp_pos, &temp_cp);
        if (temp_bytes <= 0 || temp_pos + temp_bytes > pattern_pos) break;
        last_pos = temp_pos;
        temp_pos += temp_bytes;
      }
      pattern_pos = last_pos;
      pidx++;
    }
    
    // Now scan backwards in text
    pidx--;
    text_pos = end;
    while (text_pos > start && pidx >= 0) {
      // Move back one UTF-8 character in text
      size_t temp_pos = start;
      size_t last_pos = start;
      while (temp_pos < text_pos) {
        utf8proc_int32_t temp_cp;
        utf8proc_ssize_t temp_bytes = utf8proc_iterate(
          (const utf8proc_uint8_t*)(text->data + temp_pos),
          text_pos - temp_pos, &temp_cp);
        if (temp_bytes <= 0 || temp_pos + temp_bytes > text_pos) break;
        last_pos = temp_pos;
        temp_pos += temp_bytes;
      }
      text_pos = last_pos;
      
      // Get pattern character at pidx
      size_t p_pos = 0;
      size_t p_idx = 0;
      utf8proc_int32_t pattern_cp = 0;
      while (p_pos < M && p_idx <= (size_t)pidx) {
        utf8proc_ssize_t p_bytes = utf8proc_iterate(
          (const utf8proc_uint8_t*)(pattern->data + p_pos),
          M - p_pos, &pattern_cp);
        if (p_bytes <= 0) break;
        if (p_idx == (size_t)pidx) break;
        p_pos += p_bytes;
        p_idx++;
      }
      
      // Get text character
      utf8proc_int32_t text_cp;
      utf8proc_ssize_t text_bytes = utf8proc_iterate(
        (const utf8proc_uint8_t*)(text->data + text_pos),
        end - text_pos, &text_cp);
      
      if (text_bytes > 0 && utf8_char_equal(text_cp, pattern_cp, case_sensitive, normalize)) {
        pidx--;
        if (pidx < 0) {
          start = text_pos;
          break;
        }
      }
    }
    
    int32_t score = calculate_score_utf8(case_sensitive, normalize, text, pattern,
                                         start, end, pos);
    
    // Convert byte positions to character positions
    size_t char_start = utf8_byte_to_char(char_map, start);
    size_t char_end = utf8_byte_to_char(char_map, end);
    
    // Convert position array to character positions if needed
    if (pos && pos->size > 0) {
      for (size_t i = 0; i < pos->size; i++) {
        pos->data[i] = utf8_byte_to_char(char_map, pos->data[i]);
      }
    }
    
    utf8_free_char_map(char_map);
    return (fzf_result_t){(int32_t)char_start, (int32_t)char_end, score};
  }
  
  utf8_free_char_map(char_map);
  return (fzf_result_t){-1, -1, 0};
}

fzf_result_t fzf_fuzzy_match_v2_utf8(bool case_sensitive, bool normalize,
                                     fzf_string_t *text, fzf_string_t *pattern,
                                     fzf_position_t *pos, fzf_slab_t *slab) {
  const size_t M = pattern->size;
  const size_t N = text->size;

  if (M == 0) {
    return (fzf_result_t){0, 0, 0};
  }

  // Build byte-to-char mapping for character position tracking
  utf8_char_map_t *char_map = utf8_build_char_map(text->data, N);
  if (!char_map) {
    return (fzf_result_t){-1, -1, 0};
  }

  const size_t Nc = char_map->char_count;
  const size_t Mc = utf8_strlen(pattern->data, M);

  if (Mc == 0) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){0, 0, 0};
  }

  // Fall back to v1 if slab is insufficient (use character counts)
  if (slab != NULL && Nc * Mc > slab->I16.cap) {
    utf8_free_char_map(char_map);
    return fzf_fuzzy_match_v1_utf8(case_sensitive, normalize, text, pattern,
                                   pos, slab);
  }

  // Check if pattern exists in text (returns byte position)
  int32_t tmp_idx = utf8_fuzzy_index(text, pattern->data, M, case_sensitive);
  if (tmp_idx < 0) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }

  // Convert byte position to character position
  size_t idx = utf8_byte_to_char(char_map, (size_t)tmp_idx);

  // Pre-decode pattern codepoints (case-folded if needed)
  utf8proc_int32_t *pattern_cps =
      (utf8proc_int32_t *)malloc(Mc * sizeof(utf8proc_int32_t));
  if (!pattern_cps) {
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }
  {
    size_t p_pos = 0;
    for (size_t i = 0; i < Mc; i++) {
      utf8proc_ssize_t bytes = utf8proc_iterate(
          (const utf8proc_uint8_t *)(pattern->data + p_pos), M - p_pos,
          &pattern_cps[i]);
      if (bytes <= 0) {
        free(pattern_cps);
        utf8_free_char_map(char_map);
        return (fzf_result_t){-1, -1, 0};
      }
      if (!case_sensitive) {
        pattern_cps[i] = utf8proc_case_fold(pattern_cps[i]);
      }
      p_pos += bytes;
    }
  }

  size_t offset16 = 0;
  size_t offset32 = 0;

  // Allocate arrays sized by character counts
  fzf_i16_t h0 = alloc16(&offset16, slab, Nc);
  fzf_i16_t c0 = alloc16(&offset16, slab, Nc);
  fzf_i16_t bo = alloc16(&offset16, slab, Nc);
  fzf_i32_t f = alloc32(&offset32, slab, Mc);
  fzf_i32_t t = alloc32(&offset32, slab, Nc);

  // Decode all text codepoints into t[]
  {
    size_t byte_pos = 0;
    for (size_t i = 0; i < Nc; i++) {
      utf8proc_int32_t cp;
      utf8proc_ssize_t bytes = utf8proc_iterate(
          (const utf8proc_uint8_t *)(text->data + byte_pos), N - byte_pos,
          &cp);
      if (bytes <= 0)
        break;
      t.data[i] = cp;
      byte_pos += bytes;
    }
  }

  // Phase 2: Forward scan - calculate bonus and build h0, c0, bo, f
  int16_t max_score = 0;
  size_t max_score_pos = 0;

  size_t pidx = 0;
  size_t last_idx = 0;

  utf8proc_int32_t pchar0 = pattern_cps[0];
  utf8proc_int32_t pchar = pattern_cps[0];
  int16_t prev_h0 = 0;
  int32_t prev_class = CharNonWord;
  bool in_gap = false;

  i32_slice_t t_sub = slice_i32(t.data, idx, t.size);
  i16_slice_t h0_sub =
      slice_i16_right(slice_i16(h0.data, idx, h0.size).data, t_sub.size);
  i16_slice_t c0_sub =
      slice_i16_right(slice_i16(c0.data, idx, c0.size).data, t_sub.size);
  i16_slice_t b_sub =
      slice_i16_right(slice_i16(bo.data, idx, bo.size).data, t_sub.size);

  for (size_t off = 0; off < t_sub.size; off++) {
    utf8proc_int32_t cp = t_sub.data[off];
    char_class class = char_class_of_utf8proc(cp);

    utf8proc_int32_t c = cp;
    if (!case_sensitive) {
      c = utf8proc_case_fold(cp);
    }

    t_sub.data[off] = c;
    int16_t bonus = bonus_for_utf8(prev_class, class);
    b_sub.data[off] = bonus;
    prev_class = class;

    if (c == pchar) {
      if (pidx < Mc) {
        f.data[pidx] = (int32_t)(idx + off);
        pidx++;
        pchar = pattern_cps[min64u(pidx, Mc - 1)];
      }
      last_idx = idx + off;
    }

    if (c == pchar0) {
      int16_t score = ScoreMatch + bonus * BonusFirstCharMultiplier;
      h0_sub.data[off] = score;
      c0_sub.data[off] = 1;
      if (Mc == 1 && (score > max_score)) {
        max_score = score;
        max_score_pos = idx + off;
        if (bonus == BonusBoundary) {
          break;
        }
      }
      in_gap = false;
    } else {
      if (in_gap) {
        h0_sub.data[off] = max16(prev_h0 + ScoreGapExtention, 0);
      } else {
        h0_sub.data[off] = max16(prev_h0 + ScoreGapStart, 0);
      }
      c0_sub.data[off] = 0;
      in_gap = true;
    }
    prev_h0 = h0_sub.data[off];
  }

  if (pidx != Mc) {
    free(pattern_cps);
    free_alloc(t);
    free_alloc(f);
    free_alloc(bo);
    free_alloc(c0);
    free_alloc(h0);
    utf8_free_char_map(char_map);
    return (fzf_result_t){-1, -1, 0};
  }

  if (Mc == 1) {
    free(pattern_cps);
    free_alloc(t);
    free_alloc(f);
    free_alloc(bo);
    free_alloc(c0);
    free_alloc(h0);
    utf8_free_char_map(char_map);
    fzf_result_t res = {(int32_t)max_score_pos, (int32_t)max_score_pos + 1,
                        max_score};
    append_pos(pos, max_score_pos);
    return res;
  }

  // Phase 3: DP matrix
  size_t f0 = (size_t)f.data[0];
  size_t width = last_idx - f0 + 1;
  fzf_i16_t h = alloc16(&offset16, slab, width * Mc);
  {
    i16_slice_t h0_tmp_slice = slice_i16(h0.data, f0, last_idx + 1);
    copy_into_i16(&h0_tmp_slice, &h);
  }

  fzf_i16_t c = alloc16(&offset16, slab, width * Mc);
  {
    i16_slice_t c0_tmp_slice = slice_i16(c0.data, f0, last_idx + 1);
    copy_into_i16(&c0_tmp_slice, &c);
  }

  i32_slice_t f_sub = slice_i32(f.data, 1, f.size);
  for (size_t off = 0; off < f_sub.size; off++) {
    size_t foff = (size_t)f_sub.data[off];
    pchar = pattern_cps[off + 1];
    pidx = off + 1;
    size_t row = pidx * width;
    in_gap = false;
    t_sub = slice_i32(t.data, foff, last_idx + 1);
    b_sub =
        slice_i16_right(slice_i16(bo.data, foff, bo.size).data, t_sub.size);
    i16_slice_t c_sub = slice_i16_right(
        slice_i16(c.data, row + foff - f0, c.size).data, t_sub.size);
    i16_slice_t c_diag = slice_i16_right(
        slice_i16(c.data, row + foff - f0 - 1 - width, c.size).data,
        t_sub.size);
    i16_slice_t h_sub = slice_i16_right(
        slice_i16(h.data, row + foff - f0, h.size).data, t_sub.size);
    i16_slice_t h_diag = slice_i16_right(
        slice_i16(h.data, row + foff - f0 - 1 - width, h.size).data,
        t_sub.size);
    i16_slice_t h_left = slice_i16_right(
        slice_i16(h.data, row + foff - f0 - 1, h.size).data, t_sub.size);
    h_left.data[0] = 0;

    for (size_t j = 0; j < t_sub.size; j++) {
      utf8proc_int32_t ch = t_sub.data[j];
      size_t col = j + foff;
      int16_t s1 = 0;
      int16_t s2 = 0;
      int16_t consecutive = 0;

      if (in_gap) {
        s2 = h_left.data[j] + ScoreGapExtention;
      } else {
        s2 = h_left.data[j] + ScoreGapStart;
      }

      if (pchar == ch) {
        s1 = h_diag.data[j] + ScoreMatch;
        int16_t b = b_sub.data[j];
        consecutive = c_diag.data[j] + 1;
        if (b == BonusBoundary) {
          consecutive = 1;
        } else if (consecutive > 1) {
          b = max16(b, max16(BonusConsecutive,
                             bo.data[col - ((size_t)consecutive) + 1]));
        }
        if (s1 + b < s2) {
          s1 += b_sub.data[j];
          consecutive = 0;
        } else {
          s1 += b;
        }
      }
      c_sub.data[j] = consecutive;
      in_gap = s1 < s2;
      int16_t score = max16(max16(s1, s2), 0);
      if (pidx == Mc - 1 && (score > max_score)) {
        max_score = score;
        max_score_pos = col;
      }
      h_sub.data[j] = score;
    }
  }

  // Phase 4: Backtrace
  resize_pos(pos, Mc, Mc);
  size_t j = max_score_pos;
  if (pos) {
    size_t i = Mc - 1;
    bool prefer_match = true;
    for (;;) {
      size_t ii = i * width;
      size_t j0 = j - f0;
      int16_t s = h.data[ii + j0];

      int16_t s1 = 0;
      int16_t s2 = 0;
      if (i > 0 && j >= (size_t)f.data[i]) {
        s1 = h.data[ii - width + j0 - 1];
      }
      if (j > (size_t)f.data[i]) {
        s2 = h.data[ii + j0 - 1];
      }

      if (s > s1 && (s > s2 || (s == s2 && prefer_match))) {
        unsafe_append_pos(pos, j);
        if (i == 0) {
          break;
        }
        i--;
      }
      prefer_match =
          c.data[ii + j0] > 1 ||
          (ii + width + j0 + 1 < c.size && c.data[ii + width + j0 + 1] > 0);
      j--;
    }
  }

  // Cleanup
  free(pattern_cps);
  free_alloc(h);
  free_alloc(c);
  free_alloc(t);
  free_alloc(f);
  free_alloc(bo);
  free_alloc(c0);
  free_alloc(h0);
  utf8_free_char_map(char_map);

  return (fzf_result_t){(int32_t)j, (int32_t)max_score_pos + 1,
                        (int32_t)max_score};
}

static void append_set(fzf_term_set_t *set, fzf_term_t value) {
  if (set->cap == 0) {
    set->cap = 1;
    set->ptr = (fzf_term_t *)malloc(sizeof(fzf_term_t));
  } else if (set->size + 1 > set->cap) {
    set->cap *= 2;
    set->ptr = realloc(set->ptr, sizeof(fzf_term_t) * set->cap);
  }
  set->ptr[set->size] = value;
  set->size++;
}

static void append_pattern(fzf_pattern_t *pattern, fzf_term_set_t *value) {
  if (pattern->cap == 0) {
    pattern->cap = 1;
    pattern->ptr = (fzf_term_set_t **)malloc(sizeof(fzf_term_set_t *));
  } else if (pattern->size + 1 > pattern->cap) {
    pattern->cap *= 2;
    pattern->ptr =
        realloc(pattern->ptr, sizeof(fzf_term_set_t *) * pattern->cap);
  }
  pattern->ptr[pattern->size] = value;
  pattern->size++;
}

// Helper function to get the UTF-8 version of an algorithm
static fzf_algo_t get_utf8_algo(fzf_algo_t ascii_algo) {
  if (ascii_algo == fzf_fuzzy_match_v2) return fzf_fuzzy_match_v2_utf8;
  if (ascii_algo == fzf_fuzzy_match_v1) return fzf_fuzzy_match_v1_utf8;
  if (ascii_algo == fzf_exact_match_naive) return fzf_exact_match_utf8;
  if (ascii_algo == fzf_prefix_match) return fzf_prefix_match_utf8;
  if (ascii_algo == fzf_suffix_match) return fzf_suffix_match_utf8;
  if (ascii_algo == fzf_equal_match) return fzf_equal_match_utf8;
  return ascii_algo; // fallback
}

static inline fzf_result_t call_alg_with_utf8_check(fzf_term_t *term, bool normalize,
                                                    fzf_string_t *input, fzf_position_t *pos,
                                                    fzf_slab_t *slab) {
  fzf_algo_t algo = term->fn;
  /* Check if input text contains UTF-8 */
  if (!is_ascii_utf8proc(input->data, input->size)) {
    /* Get UTF-8 version of the algorithm */
    algo = get_utf8_algo(algo);
  }
  return algo(term->case_sensitive, normalize, input,
              (fzf_string_t *)term->text, pos, slab);
}

#define CALL_ALG(term, normalize, input, pos, slab) \
  call_alg_with_utf8_check(term, normalize, &(input), pos, slab)

// TODO(conni2461): REFACTOR
/* assumption (maybe i change that later)
 * - always v2 alg
 * - bool extended always true (thats the whole point of this isn't it)
 */
fzf_pattern_t *fzf_parse_pattern(fzf_case_types case_mode, bool normalize,
                                 char *pattern, bool fuzzy) {
  fzf_pattern_t *pat_obj = (fzf_pattern_t *)malloc(sizeof(fzf_pattern_t));
  memset(pat_obj, 0, sizeof(*pat_obj));

  size_t pat_len = strlen(pattern);
  if (pat_len == 0) {
    return pat_obj;
  }
  pattern = trim_whitespace_left(pattern, &pat_len);
  while (has_suffix(pattern, pat_len, " ", 1) &&
         !has_suffix(pattern, pat_len, "\\ ", 2)) {
    pattern[pat_len - 1] = 0;
    pat_len--;
  }

  char *pattern_copy = str_replace(pattern, "\\ ", "\t");
  const char *delim = " ";
  char *ptr = strtok(pattern_copy, delim);

  fzf_term_set_t *set = (fzf_term_set_t *)malloc(sizeof(fzf_term_set_t));
  memset(set, 0, sizeof(*set));

  bool switch_set = false;
  bool after_bar = false;
  while (ptr != NULL) {
    fzf_algo_t fn = fzf_fuzzy_match_v2;
    bool inv = false;

    size_t len = strlen(ptr);
    str_replace_char(ptr, '\t', ' ');
    char *text = strdup(ptr);

    char *og_str = text;
    char *lower_text = str_tolower(text, len);
    bool case_sensitive =
        case_mode == CaseRespect ||
        (case_mode == CaseSmart && strcmp(text, lower_text) != 0);
    if (!case_sensitive) {
      SFREE(text);
      text = lower_text;
      og_str = lower_text;
    } else {
      SFREE(lower_text);
    }
    
    // Check if pattern contains UTF-8 characters
    bool is_utf8 = !is_ascii_utf8proc(text, len);
    
    if (!fuzzy) {
      fn = is_utf8 ? fzf_exact_match_utf8 : fzf_exact_match_naive;
    } else {
      fn = is_utf8 ? fzf_fuzzy_match_v2_utf8 : fzf_fuzzy_match_v2;
    }
    if (set->size > 0 && !after_bar && strcmp(text, "|") == 0) {
      switch_set = false;
      after_bar = true;
      ptr = strtok(NULL, delim);
      SFREE(og_str);
      continue;
    }
    after_bar = false;
    if (has_prefix(text, "!", 1)) {
      inv = true;
      fn = is_utf8 ? fzf_exact_match_utf8 : fzf_exact_match_naive;
      text++;
      len--;
    }

    if (strcmp(text, "$") != 0 && has_suffix(text, len, "$", 1)) {
      fn = is_utf8 ? fzf_suffix_match_utf8 : fzf_suffix_match;
      text[len - 1] = 0;
      len--;
    }

    if (has_prefix(text, "'", 1)) {
      if (fuzzy && !inv) {
        fn = is_utf8 ? fzf_exact_match_utf8 : fzf_exact_match_naive;
        text++;
        len--;
      } else {
        fn = is_utf8 ? fzf_fuzzy_match_v2_utf8 : fzf_fuzzy_match_v2;
        text++;
        len--;
      }
    } else if (has_prefix(text, "^", 1)) {
      if (fn == (is_utf8 ? fzf_suffix_match_utf8 : fzf_suffix_match)) {
        fn = is_utf8 ? fzf_equal_match_utf8 : fzf_equal_match;
      } else {
        fn = is_utf8 ? fzf_prefix_match_utf8 : fzf_prefix_match;
      }
      text++;
      len--;
    }

    if (len > 0) {
      if (switch_set) {
        append_pattern(pat_obj, set);
        set = (fzf_term_set_t *)malloc(sizeof(fzf_term_set_t));
        set->cap = 0;
        set->size = 0;
      }
      fzf_string_t *text_ptr = (fzf_string_t *)malloc(sizeof(fzf_string_t));
      text_ptr->data = text;
      text_ptr->size = len;
      append_set(set, (fzf_term_t){.fn = fn,
                                   .inv = inv,
                                   .ptr = og_str,
                                   .text = text_ptr,
                                   .case_sensitive = case_sensitive});
      switch_set = true;
    } else {
      SFREE(og_str);
    }

    ptr = strtok(NULL, delim);
  }
  if (set->size > 0) {
    append_pattern(pat_obj, set);
  } else {
    SFREE(set->ptr);
    SFREE(set);
  }
  bool only = true;
  for (size_t i = 0; i < pat_obj->size; i++) {
    fzf_term_set_t *term_set = pat_obj->ptr[i];
    if (term_set->size > 1) {
      only = false;
      break;
    }
    if (term_set->ptr[0].inv == false) {
      only = false;
      break;
    }
  }
  pat_obj->only_inv = only;
  SFREE(pattern_copy);
  return pat_obj;
}

void fzf_free_pattern(fzf_pattern_t *pattern) {
  if (pattern->ptr) {
    for (size_t i = 0; i < pattern->size; i++) {
      fzf_term_set_t *term_set = pattern->ptr[i];
      for (size_t j = 0; j < term_set->size; j++) {
        fzf_term_t *term = &term_set->ptr[j];
        free(term->ptr);
        free(term->text);
      }
      free(term_set->ptr);
      free(term_set);
    }
    free(pattern->ptr);
  }
  SFREE(pattern);
}

int32_t fzf_get_score(const char *text, fzf_pattern_t *pattern,
                      fzf_slab_t *slab) {
  // If the pattern is an empty string then pattern->ptr will be NULL and we
  // basically don't want to filter. Return 1 for telescope
  if (pattern->ptr == NULL) {
    return 1;
  }

  fzf_string_t input = {.data = text, .size = strlen(text)};
  if (pattern->only_inv) {
    int final = 0;
    for (size_t i = 0; i < pattern->size; i++) {
      fzf_term_set_t *term_set = pattern->ptr[i];
      fzf_term_t *term = &term_set->ptr[0];

      final += CALL_ALG(term, false, input, NULL, slab).score;
    }
    return (final > 0) ? 0 : 1;
  }

  int32_t total_score = 0;
  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *term_set = pattern->ptr[i];
    int32_t current_score = 0;
    bool matched = false;
    for (size_t j = 0; j < term_set->size; j++) {
      fzf_term_t *term = &term_set->ptr[j];
      fzf_result_t res = CALL_ALG(term, false, input, NULL, slab);
      if (res.start >= 0) {
        if (term->inv) {
          continue;
        }
        current_score = res.score;
        matched = true;
        break;
      }

      if (term->inv) {
        current_score = 0;
        matched = true;
      }
    }
    if (matched) {
      total_score += current_score;
    } else {
      total_score = 0;
      break;
    }
  }

  return total_score;
}

fzf_position_t *fzf_get_positions(const char *text, fzf_pattern_t *pattern,
                                  fzf_slab_t *slab) {
  // If the pattern is an empty string then pattern->ptr will be NULL and we
  // basically don't want to filter. Return 1 for telescope
  if (pattern->ptr == NULL) {
    return NULL;
  }

  fzf_string_t input = {.data = text, .size = strlen(text)};
  fzf_position_t *all_pos = fzf_pos_array(0);
  for (size_t i = 0; i < pattern->size; i++) {
    fzf_term_set_t *term_set = pattern->ptr[i];
    bool matched = false;
    for (size_t j = 0; j < term_set->size; j++) {
      fzf_term_t *term = &term_set->ptr[j];
      if (term->inv) {
        // If we have an inverse term we need to check if we have a match, but
        // we are not interested in the positions (for highlights) so to speed
        // this up we can pass in NULL here and don't calculate the positions
        fzf_result_t res = CALL_ALG(term, false, input, NULL, slab);
        if (res.start < 0) {
          matched = true;
        }
        continue;
      }
      fzf_result_t res = CALL_ALG(term, false, input, all_pos, slab);
      if (res.start >= 0) {
        matched = true;
        break;
      }
    }
    if (!matched) {
      fzf_free_positions(all_pos);
      return NULL;
    }
  }
  return all_pos;
}

void fzf_free_positions(fzf_position_t *pos) {
  if (pos) {
    SFREE(pos->data);
    free(pos);
  }
}

fzf_slab_t *fzf_make_slab(fzf_slab_config_t config) {
  fzf_slab_t *slab = (fzf_slab_t *)malloc(sizeof(fzf_slab_t));
  memset(slab, 0, sizeof(*slab));

  slab->I16.data = (int16_t *)malloc(config.size_16 * sizeof(int16_t));
  memset(slab->I16.data, 0, config.size_16 * sizeof(*slab->I16.data));
  slab->I16.cap = config.size_16;
  slab->I16.size = 0;
  slab->I16.allocated = true;

  slab->I32.data = (int32_t *)malloc(config.size_32 * sizeof(int32_t));
  memset(slab->I32.data, 0, config.size_32 * sizeof(*slab->I32.data));
  slab->I32.cap = config.size_32;
  slab->I32.size = 0;
  slab->I32.allocated = true;

  return slab;
}

fzf_slab_t *fzf_make_default_slab(void) {
  return fzf_make_slab((fzf_slab_config_t){(size_t)100 * 1024, 2048});
}

void fzf_free_slab(fzf_slab_t *slab) {
  if (slab) {
    free(slab->I16.data);
    free(slab->I32.data);
    free(slab);
  }
}