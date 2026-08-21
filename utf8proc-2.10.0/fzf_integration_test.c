#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utf8proc.h"

void test_case_folding() {
    printf("=== Testing Case Folding (for case-insensitive search) ===\n");
    
    const char* test_strings[] = {
        "Hello World",
        "ÑOÑO",
        "αβγδε",
        "Pokémon",
        "ポケモン",
        "🔥Fire🔥",
        NULL
    };
    
    for (int i = 0; test_strings[i] != NULL; i++) {
        utf8proc_uint8_t *result;
        utf8proc_ssize_t len = utf8proc_map(
            (const utf8proc_uint8_t*)test_strings[i], 0, &result,
            UTF8PROC_NULLTERM | UTF8PROC_CASEFOLD | UTF8PROC_COMPOSE | UTF8PROC_STABLE
        );
        
        if (len >= 0) {
            printf("  \"%s\" -> \"%.*s\" (len=%ld)\n", test_strings[i], (int)len, result, len);
            free(result);
        } else {
            printf("  Error processing \"%s\": %s\n", test_strings[i], utf8proc_errmsg(len));
        }
    }
    printf("\n");
}

void test_character_iteration() {
    printf("=== Testing Character Iteration ===\n");
    
    const char* test_str = "Hello 世界 🌍";
    printf("String: \"%s\"\n", test_str);
    printf("Characters:\n");
    
    const utf8proc_uint8_t *str = (const utf8proc_uint8_t*)test_str;
    utf8proc_ssize_t pos = 0;
    utf8proc_int32_t codepoint;
    int char_count = 0;
    
    while (pos < (utf8proc_ssize_t)strlen(test_str)) {
        utf8proc_ssize_t bytes_read = utf8proc_iterate(str + pos, -1, &codepoint);
        if (bytes_read < 0) {
            printf("  Error at position %ld: %s\n", pos, utf8proc_errmsg(bytes_read));
            break;
        }
        
        printf("  [%d] U+%04X (%ld bytes)\n", char_count, codepoint, bytes_read);
        pos += bytes_read;
        char_count++;
    }
    printf("Total characters: %d\n\n", char_count);
}

void test_character_properties() {
    printf("=== Testing Character Properties ===\n");
    
    utf8proc_int32_t test_chars[] = {
        'A', 'a', '1', ' ', '\n',
        0x00D1, // Ñ
        0x03B1, // α (Greek alpha)
        0x4E16, // 世 (Chinese character)
        0x1F525, // 🔥 (fire emoji)
        0
    };
    
    for (int i = 0; test_chars[i] != 0; i++) {
        utf8proc_int32_t cp = test_chars[i];
        const utf8proc_property_t *prop = utf8proc_get_property(cp);
        
        printf("  U+%04X:", cp);
        if (prop->category == UTF8PROC_CATEGORY_LU) printf(" Uppercase");
        if (prop->category == UTF8PROC_CATEGORY_LL) printf(" Lowercase");
        if (prop->category == UTF8PROC_CATEGORY_ND) printf(" Digit");
        if (prop->category == UTF8PROC_CATEGORY_ZS) printf(" Space");
        if (prop->category == UTF8PROC_CATEGORY_CC) printf(" Control");
        if (prop->category == UTF8PROC_CATEGORY_SO) printf(" Symbol");
        printf(" (width: %d)\n", prop->charwidth);
    }
    printf("\n");
}

int main() {
    printf("UTF8PROC Integration Test for FZF\n");
    printf("Version: %s\n", utf8proc_version());
    printf("Unicode Version: %s\n\n", utf8proc_unicode_version());
    
    test_case_folding();
    test_character_iteration();
    test_character_properties();
    
    printf("All tests completed successfully!\n");
    return 0;
} 