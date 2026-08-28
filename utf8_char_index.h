/*
 * UTF-8 Character Indexing Utilities for fzf-native
 * Provides efficient byte-to-character position mapping using utf8proc
 */

#ifndef UTF8_CHAR_INDEX_H
#define UTF8_CHAR_INDEX_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include "utf8proc-2.10.0/utf8proc.h"

// Mapping structure for byte-to-char conversion
typedef struct {
    size_t *byte_to_char;  // Array: byte_index → char_index
    size_t byte_count;      // Total bytes in string
    size_t char_count;      // Total characters in string
    bool owned;             // The array and this struct must be freed
} utf8_char_map_t;

/* One scorer slab belongs to one scoring thread at a time.  Keep its mapping
   array across ordinary candidates so UTF-8 scoring does not allocate and
   free per candidate.  The retained allocation is bounded below; unusually
   large candidates use an owned one-shot map instead of increasing every
   worker's high-water memory indefinitely. */
typedef struct {
    utf8_char_map_t map;
    size_t byte_slot_capacity;
} utf8_char_map_scratch_t;

/* Retain at most 1 MiB per slab.  This is mapping allocation size, not input
   length; on a 64-bit build it covers candidates through roughly 128 KiB. */
#define FZF_UTF8_CHAR_MAP_RETAINED_BYTES_MAX ((size_t)1024 * 1024)

/* Decode one UTF-8 unit at BUF (LEN > 0 bytes remaining).  Valid
   sequences decode normally.  An undecodable byte decodes as a
   single-byte unit whose codepoint is 0xDC00+byte (the surrogate-escape
   convention): it can never equal a codepoint decoded from valid UTF-8,
   the same raw byte on both sides of a comparison still matches itself,
   byte->char tables stay monotonic, and the one-char-per-raw-byte
   counting agrees with how Emacs counts these strings.  utf8proc's
   case/category functions pass surrogates through unchanged.  Never
   returns a value < 1, so every decode loop makes progress. */
static inline utf8proc_ssize_t utf8_iterate_lossy(
    const utf8proc_uint8_t *buf, utf8proc_ssize_t len,
    utf8proc_int32_t *cp) {
    utf8proc_ssize_t bytes = utf8proc_iterate(buf, len, cp);
    if (bytes <= 0 || *cp < 0) {
        *cp = 0xDC00 + (utf8proc_int32_t)buf[0];
        return 1;
    }
    return bytes;
}

/* Create a byte-to-char mapping for a UTF-8 string.  SCRATCH may be NULL.
   A returned map with owned=false aliases SCRATCH and remains valid only
   until that same scratch is reused or destroyed.  An owned=true map is
   independent and must be passed to utf8_free_char_map.  Returns NULL on
   allocation failure without releasing existing scratch storage. */
static inline utf8_char_map_t* utf8_build_char_map(
    const char *str, size_t byte_len, utf8_char_map_scratch_t *scratch) {
    if (byte_len == SIZE_MAX) return NULL;
    size_t byte_slots = byte_len + 1;
    if (byte_slots > SIZE_MAX / sizeof(size_t)) return NULL;
    size_t map_bytes = byte_slots * sizeof(size_t);
    bool retain = scratch &&
        map_bytes <= FZF_UTF8_CHAR_MAP_RETAINED_BYTES_MAX;
    utf8_char_map_t *map = retain ? &scratch->map : calloc(1, sizeof *map);
    if (!map) return NULL;
    
    // First pass: count characters
    const uint8_t *ptr = (const uint8_t *)str;
    size_t byte_pos = 0;
    size_t char_count = 0;
    
    while (byte_pos < byte_len) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t bytes = utf8_iterate_lossy(ptr + byte_pos, byte_len - byte_pos, &cp);
        byte_pos += bytes;
        if (char_count == SIZE_MAX) {
            if (!retain) free(map);
            return NULL;
        }
        char_count++;
    }

    // Allocate or grow the mapping array.  Every used element is overwritten, so
    // zero-initialization is unnecessary.
    if (retain) {
        if (scratch->byte_slot_capacity < byte_slots) {
            size_t *next = realloc(map->byte_to_char, map_bytes);
            if (!next) return NULL;
            map->byte_to_char = next;
            scratch->byte_slot_capacity = byte_slots;
        }
        map->owned = false;
    } else {
        map->byte_to_char = malloc(map_bytes);
        map->owned = true;
        if (!map->byte_to_char) {
            free(map);
            return NULL;
        }
    }
    
    map->byte_count = byte_len;
    map->char_count = char_count;
    
    // Second pass: build mappings
    byte_pos = 0;
    size_t char_pos = 0;
    
    while (byte_pos < byte_len) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t bytes = utf8_iterate_lossy(ptr + byte_pos, byte_len - byte_pos, &cp);
        
        // All bytes in this character map to the same char position
        // bytes is guaranteed >= 1, so the cast to size_t cannot wrap.
        for (size_t i = 0; i < (size_t)bytes; i++) {
            map->byte_to_char[byte_pos + i] = char_pos;
        }
        
        byte_pos += bytes;
        char_pos++;
    }
    
    // Handle end positions
    map->byte_to_char[byte_len] = char_count;
    
    return map;
}

// Free an independent map.  A scratch-owned map is released with its slab.
static inline void utf8_free_char_map(utf8_char_map_t *map) {
    if (map && map->owned) {
        free(map->byte_to_char);
        free(map);
    }
}

// Convert byte position to character position
static inline size_t utf8_byte_to_char(utf8_char_map_t *map, size_t byte_pos) {
    if (!map || byte_pos > map->byte_count) return 0;
    return map->byte_to_char[byte_pos];
}

// Count UTF-8 characters in string
static inline size_t utf8_strlen(const char *str, size_t byte_len) {
    const uint8_t *ptr = (const uint8_t *)str;
    size_t byte_pos = 0;
    size_t char_count = 0;
    
    while (byte_pos < byte_len) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t bytes = utf8_iterate_lossy(ptr + byte_pos, byte_len - byte_pos, &cp);
        byte_pos += bytes;
        char_count++;
    }
    
    return char_count;
}

#endif // UTF8_CHAR_INDEX_H
