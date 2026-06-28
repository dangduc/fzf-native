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

// UTF-8 string iterator with position tracking
typedef struct {
    const uint8_t *data;    // UTF-8 string data
    size_t byte_pos;        // Current byte position
    size_t char_pos;        // Current character position
    size_t byte_len;        // Total byte length
    utf8proc_int32_t current; // Current codepoint
} utf8_iter_t;

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
        utf8proc_ssize_t bytes = utf8proc_iterate(ptr + byte_pos, byte_len - byte_pos, &cp);
        if (bytes <= 0) break;
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
        utf8proc_ssize_t bytes = utf8proc_iterate(ptr + byte_pos, byte_len - byte_pos, &cp);
        if (bytes <= 0) break;
        
        // All bytes in this character map to the same char position
        for (size_t i = 0; i < bytes; i++) {
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

// Convert character position to byte position
static inline size_t utf8_char_to_byte(utf8_char_map_t *map, size_t char_pos) {
    if (!map || char_pos > map->char_count) return 0;
    return map->char_to_byte[char_pos];
}

// Initialize UTF-8 iterator
static inline void utf8_iter_init(utf8_iter_t *iter, const char *str, size_t byte_len) {
    iter->data = (const uint8_t *)str;
    iter->byte_pos = 0;
    iter->char_pos = 0;
    iter->byte_len = byte_len;
    iter->current = 0;
}

// Advance iterator to next character
// Returns false at end of string
static inline bool utf8_iter_next(utf8_iter_t *iter) {
    if (iter->byte_pos >= iter->byte_len) {
        return false;
    }
    
    utf8proc_ssize_t bytes = utf8proc_iterate(
        iter->data + iter->byte_pos,
        iter->byte_len - iter->byte_pos,
        &iter->current
    );
    
    if (bytes <= 0) {
        return false;
    }
    
    iter->byte_pos += bytes;
    iter->char_pos++;
    return true;
}

// Get current character's byte position
static inline size_t utf8_iter_byte_pos(utf8_iter_t *iter) {
    // Return start of current character (before last advance)
    const uint8_t *ptr = iter->data;
    size_t pos = 0;
    size_t char_idx = 0;
    
    while (char_idx < iter->char_pos - 1 && pos < iter->byte_len) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t bytes = utf8proc_iterate(ptr + pos, iter->byte_len - pos, &cp);
        if (bytes <= 0) break;
        pos += bytes;
        char_idx++;
    }
    
    return pos;
}

// Check if string is pure ASCII (fast path optimization)
static inline bool utf8_is_ascii(const char *str, size_t byte_len) {
    for (size_t i = 0; i < byte_len; i++) {
        if ((unsigned char)str[i] >= 128) {
            return false;
        }
    }
    return true;
}

// Count UTF-8 characters in string
static inline size_t utf8_strlen(const char *str, size_t byte_len) {
    const uint8_t *ptr = (const uint8_t *)str;
    size_t byte_pos = 0;
    size_t char_count = 0;
    
    while (byte_pos < byte_len) {
        utf8proc_int32_t cp;
        utf8proc_ssize_t bytes = utf8proc_iterate(ptr + byte_pos, byte_len - byte_pos, &cp);
        if (bytes <= 0) break;
        byte_pos += bytes;
        char_count++;
    }
    
    return char_count;
}

#endif // UTF8_CHAR_INDEX_H