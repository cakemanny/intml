#ifndef __SYMBOLS_H__
#define __SYMBOLS_H__

typedef const char* Symbol;

/**
 * Adds the string the to the symbol table (making a copy) if missing
 * Returns the pointer from the symbol table if matching string found
 */
Symbol symbol(const char*);


#endif /* __SYMBOLS_H__ */
