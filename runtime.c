#include "ast.h"
#include <stdlib.h>

/*
 * It might be interesting to try implement green threads, as per this article
 * and code sample:
 *
 *      https://c9x.me/articles/gthreads/code0.html
 *      https://github.com/mpu/gthreads/tree/code0
 */

#define WORD_SIZE (sizeof(void*))

static struct SizeAlloc {
    size_t size;
    int count;
} sizes_allocated[1024];

void end_of_program()
{
    int len = 1;
    for (; len < 1024 && sizes_allocated[len].size > 0; len++);

    fprintf(stdout, "Allocation Distribution:\n");
    fprintf(stdout, "+--------+---------+\n");
    fprintf(stdout, "|  size  |  count  |\n");
    fprintf(stdout, "+--------+---------+\n");
    for (int i = 0; i < len; i++) {
        fprintf(stdout, "| %6lu | %7u |\n", sizes_allocated[i].size, sizes_allocated[i].count);
    }
    fprintf(stdout, "+--------+---------+\n");
}

// for the time being, just call malloc
void* ml_gc_alloc(size_t size)
{
    static int first_time = 1;
    if (first_time) {
        first_time = 0;
        //atexit(end_of_program);
    }

    if (size == 0) {
        sizes_allocated[0].count++;
    } else {
        for (int i = 1; i < 1024; i++) {
            if (sizes_allocated[i].size == 0) {
                // Need to go back and add us in
                sizes_allocated[i].size = size;
                sizes_allocated[i].count = 1;
                break;
            }
            if (sizes_allocated[i].size == size) {
                sizes_allocated[i].count++;
                break;
            }
        }
    }
    return malloc(size);
}
