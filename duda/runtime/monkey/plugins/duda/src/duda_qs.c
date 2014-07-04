/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "MKPlugin.h"
#include "duda.h"
#include "duda_gc.h"
#include "duda_qs.h"
#include "duda_qs_map.h"

/*
 * @OBJ_NAME: qs
 * @OBJ_MENU: Query String
 * @OBJ_DESC: The query string object provides a set of methods to manipulate the
 * incoming information from the query string URL section.
 */

struct duda_api_qs *duda_qs_object()
{
    struct duda_api_qs *qs;

    qs = mk_api->mem_alloc(sizeof(struct duda_api_qs));
    qs->count  = duda_qs_count;
    qs->get    = duda_qs_get;
    qs->get_id = duda_qs_get_id;
    qs->cmp    = duda_qs_cmp;

    return qs;
};


/*
 * @METHOD_NAME: count
 * @METHOD_DESC: It returns the number of valid parameters given in the query
 * string. A valid parameter is composed by a key and a value.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: The number of valid parameters.
 */
int duda_qs_count(duda_request_t *dr)
{
    return dr->qs.count;
}

/*
 * @METHOD_NAME: get
 * @METHOD_DESC: Return a new buffer with the value of the given key from the
 * query string. The new buffer is automatically freed once the service finish it
 * works.
 * @METHOD_PROTO: char *get(duda_request_t *dr, const char *key);
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: key the name of the key
 * @METHOD_RETURN: Upon successful completion it returns the new memory buffer
 * with the value of the key specified, on error returns NULL.
 */
char *duda_qs_get(duda_request_t *dr, const char *key)
{
    int i;
    unsigned int len;
    char *value = NULL;

    if (dr->qs.count <= 0) {
        return NULL;
    }

    len = strlen(key);
    for (i=0 ; i < dr->qs.count; i++) {
        if (dr->qs.entries[i].key.len == len &&
            strncmp(dr->qs.entries[i].key.data, key, len) == 0) {
            value = mk_api->str_copy_substr(dr->qs.entries[i].value.data, 0,
                                            (int) dr->qs.entries[i].value.len);

            /* Register the new memory buffer into the garbage collector */
            duda_gc_add(dr, value);
            return value;
        }
    }

    return NULL;
}

/*
 * @METHOD_NAME: get_id
 * @METHOD_DESC: Lookup a query string variable given it ID or numeric position
 * (starting from zero). It will return a new buffer with the value of the given
 * key. The new buffer is automatically freed once the service finish it works.
 * @METHOD_PROTO: char *get_id(duda_request_t *dr, int idx);
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: idx the variable ID or numeric position.
 * @METHOD_RETURN: Upon successful completion it returns the new memory buffer
 * with the value of the key specified, on error returns NULL.
 */
char *duda_qs_get_id(duda_request_t *dr, int idx)
{
    char *value = NULL;

    if (dr->qs.count <= 0) {
        return NULL;
    }

    if (idx >= dr->qs.count) {
        return NULL;
    }

    value = mk_api->str_copy_substr(dr->qs.entries[idx].value.data, 0,
                                    (int) dr->qs.entries[idx].value.len);

    /* Register the new memory buffer into the garbage collector */
    duda_gc_add(dr, value);
    return value;
}

/*
 * @METHOD_NAME: cmp
 * @METHOD_DESC: It lookup and compares a given key and value string with the
 * incoming parameters from the query string.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: key the name of the key
 * @METHOD_PARAM: value the value of the key to compare
 * @METHOD_RETURN: If the key exists and the value matches, it returns 0,
 * on error it returns -1.
 */
int duda_qs_cmp(duda_request_t *dr, const char *key, const char *value)
{
    int i;
    struct duda_qs_map *qs = &dr->qs;

    if (dr->qs.count <= 0) {
        return -1;
    }

    for (i=0 ; i < dr->qs.count; i++) {
        if (strncmp(qs->entries[i].key.data, key,
                    qs->entries[i].key.len) == 0) {

            /* Compare the key value */
            if (strncmp(qs->entries[i].value.data, value,
                        qs->entries[i].value.len) == 0) {
                return 0;
            }
        }
    }

    return -1;
}

/*
 * Query string parser, if the query string section exists, it will parse the
 * content and fill the entries mapping the keys and values.
 */
int duda_qs_parse(duda_request_t *dr)
{
    int i;
    int len   = 0;
    int qs_len;
    int count = 0;
    char *key = NULL;
    char *val = NULL;
    struct duda_qs_map *qs = &dr->qs;
    struct session_request *sr = dr->sr;

    /* If we do not have a query string just return */
    if (!sr->query_string.data) {
        return -1;
    }

    qs_len = sr->query_string.len;

    /* Start parsing the query string */
    for (i=0 ; i < qs_len; i++ ) {
        switch (sr->query_string.data[i]) {
        case '&':
            if (key && val) {
                len = (i - (val - sr->query_string.data));
                qs->entries[count].value.data = val + 1;
                qs->entries[count].value.len  = len - 1;
                count++;
            }

            len = 0;
            key = NULL;
            val = NULL;
            break;
        case '=':
            /* check if the key has finished */
            if (key && !val) {
                /* set the key */
                len = (i - (key - sr->query_string.data));
                qs->entries[count].key.data = key;
                qs->entries[count].key.len  = len;

                /* set the value start */
                if (sr->query_string.data[i] != ' ') {
                    val = sr->query_string.data + i;
                }
            }
            break;
        default:
            /* A new key is starting */
            if (!key) {
                key = sr->query_string.data + i;
            }
        };
    }

    if (key && val) {
        len = (i - (val - sr->query_string.data));
        qs->entries[count].value.data = val + 1;
        qs->entries[count].value.len  = len - 1;
        count++;
    }

    qs->count = count;

    /* DEBUG
    key = malloc(sr->query_string.len + 1);
    strncpy(key, sr->query_string.data, sr->query_string.len);
    key[sr->query_string.len] = '\0';

    printf("query string = '%s'\n", key);
    free(key);

    for (i=0; i < qs->count; i++) {
        key = malloc(qs->entries[i].key.len + 1);
        strncpy(key, qs->entries[i].key.data, qs->entries[i].key.len);
        key[qs->entries[i].key.len] = '\0';

        val = malloc(qs->entries[i].value.len + 1);
        strncpy(val, qs->entries[i].value.data, qs->entries[i].value.len);
        val[qs->entries[i].value.len] = '\0';

        printf("key='%s' ; val='%s'\n", key, val);
        free(key);
        free(val);
    }
    */

    return count;
}
