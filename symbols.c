/*
 * symbols.c
 * A very basic implementation of a symbol table
 */
#include "symbols.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static Symbol default_table[1024];

static Symbol* table = default_table;
static size_t table_capcity = 1024;
static size_t table_len = 0;


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
            perror("symbols: out of memory");
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

SymList* symbol_list(Symbol first)
{
    SymList* list = malloc(sizeof *list);
    if (!list) {
        perror("symbols: out of memory");
        abort();
    }
    list->name = first;
    list->next = NULL;
    return list;
}

SymList* symbol_list_add(SymList* list, Symbol node)
{
    SymList* result = symbol_list(node);
    result->next = list;
    return result;
}

void symbol_list_free(SymList* list)
{
    SymList* head = list;
    while (head) {
        SymList* next = head->next;
        free(head);
        head = next;
    }
}



