/*
 * symbols.c
 * A very basic implementation of a symbol table
 */
#include "symbols.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

Symbol default_table[1024];

Symbol* table = default_table;
size_t table_capcity = 1024;
size_t table_len = 0;


Symbol symbol(const char* str)
{
    for (int i = 0; i < table_len; i++) {
        if (str == table[i] || strcmp(str, table[i]) == 0) {
            return table[i];
        }
    }
    if (!(table_len < table_capcity)) {
        // reallocate
        Symbol* tmp = calloc(2 * table_capcity, sizeof *table);
        if (!tmp) {
            fprintf(stderr, "symbols: out of memory\n");
            abort();
        }
        memcpy(tmp, table, table_capcity * sizeof *table);

        if (table != default_table) {
            free(table);
        }
        table = tmp;
        table_capcity = 2 * table_capcity;
    }
    return table[table_len++] = strdup(str);
}


