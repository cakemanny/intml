#include "ast.h"
#include <stdlib.h>


/*
 * Need a garbage collector in here
 */

// for the time being, just call malloc
void* ml_gc_alloc(size_t size)
{
    return malloc(size);
}


