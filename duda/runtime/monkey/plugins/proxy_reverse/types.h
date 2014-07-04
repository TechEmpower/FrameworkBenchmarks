#ifndef _TYPES_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "MKPlugin.h"

/* System resources are not sufficient to handle the request. */
#define ERROR_MEMORY                -1

/* Entry is missing. */
#define ERROR_MISSING               -2

/* Entry exists. */
#define ERROR_EXIST                 -3

/* Unknown error. */
#define ERROR                       -32767

/* String literal. Contains length and pointer to the data. The data is usually NUL-terminated so that it can be passed to standard functions without modification. */
struct string
{
    char *data;
    size_t length;
};

/*
Generates string literal from data and length. If no length is passed, it assumes that string data is static array and determines its length with sizeof.
Examples:
 struct string name = string(name_data, name_length);
 struct string key = string("uuid");
*/
#define _mkpr_string(data, length, ...) {(data), (length)}
#define string(...) _mkpr_string(__VA_ARGS__, sizeof(__VA_ARGS__) - 1)

/* Creates a new string and sets its data and length accordingly. The string must be freed with free(). */
struct string *string_alloc(const char *data, size_t length);

/* Dictionary */

#define DICT_SIZE_BASE 16

struct dict
{
    struct dict_item
    {
        size_t key_size;
        const char *key_data;
        void *value;
        struct dict_item *_next;
    }       **items;
    size_t count, size;
};

struct dict_iterator
{
    size_t index;
    struct dict_item *item;
};

/* Initializes dictionary iterator and returns the first item. */
const struct dict_item *dict_first(struct dict_iterator *it,
                                   const struct dict *d);
/* Returns next item in a dictionary iterator. */
const struct dict_item *dict_next(struct dict_iterator *it,
                                  const struct dict *d);

/* WARNING: size must be a power of 2 */
bool dict_init(struct dict *dict, size_t size);

int dict_set(struct dict *dict, const struct string *key, void *value,
             void **result);
#define dict_add(dict, key, value) dict_set((dict), (key), (value), 0)

void *dict_get(const struct dict *dict, const struct string *key);

void *dict_remove(struct dict *dict, const struct string *key);

void dict_term(struct dict *dict);

#define _TYPES_H
#endif
