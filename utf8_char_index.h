/*
 * UTF-8 Character Indexing Utilities for fzf-native
 * Provides efficient byte↔character position mapping using utf8proc
 */

#ifndef UTF8_CHAR_INDEX_H
#define UTF8_CHAR_INDEX_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include "utf8proc.h"

// Mapping structure for byte↔char conversion
typedef struct {
    size_t *byte_to_char;  // Array: byte_index → char_index
    size_t *char_to_byte;  // Array: char_index → byte_index  
    size_t byte_count;      // Total bytes in string
    size_t char_count;      // Total characters in string
} utf8_char_map_t;

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

// Create a byte↔char mapping for a UTF-8 string
// Returns NULL on allocation failure
static inline utf8_char_map_t* utf8_build_char_map(const char *str, size_t byte_len) {
    utf8_char_map_t *map = malloc(sizeof(utf8_char_map_t));
    if (!map) return NULL;
    
    // First pass: count characters
    const uint8_t *ptr = (const uint8_t *)str;
    size_t byte_pos = 0;
    size_t char_count = 0;
    
    while (byte_pos < byte_len) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t bytes = utf8_iterate_lossy(ptr + byte_pos, byte_len - byte_pos, &cp);
        byte_pos += bytes;
        char_count++;
    }
    
    // Allocate mapping arrays
    map->byte_to_char = calloc(byte_len + 1, sizeof(size_t));
    map->char_to_byte = calloc(char_count + 1, sizeof(size_t));
    
    if (!map->byte_to_char || !map->char_to_byte) {
        free(map->byte_to_char);
        free(map->char_to_byte);
        free(map);
        return NULL;
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
        
        // Character position maps to start byte
        map->char_to_byte[char_pos] = byte_pos;
        
        byte_pos += bytes;
        char_pos++;
    }
    
    // Handle end positions
    map->byte_to_char[byte_len] = char_count;
    map->char_to_byte[char_count] = byte_len;
    
    return map;
}

// Free a character map
static inline void utf8_free_char_map(utf8_char_map_t *map) {
    if (map) {
        free(map->byte_to_char);
        free(map->char_to_byte);
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