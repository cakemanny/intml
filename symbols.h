#ifndef __SYMBOLS_H__
#define __SYMBOLS_H__

typedef const char* Symbol;

/**
 * Adds the string the to the symbol table (making a copy) if missing
 * Returns the pointer from the symbol table if matching string found
 */
Symbol symbol(const char*);


typedef struct SymList {
    Symbol name;
    struct SymList* next;
} SymList;

/*
 * Constructs a new symbol list with one item
 */
SymList* symbol_list(Symbol first);

/*
 * Add to head of list and return the new head
 */
SymList* symbol_list_add(SymList* list, Symbol node);

// Free the list (but not the items in the list)
void symbol_list_free(SymList* list);

#endif /* __SYMBOLS_H__ */
