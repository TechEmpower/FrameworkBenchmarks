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

#include "mk_string.h"
#include "duda_api.h"

#ifndef MK_DUDA_FCONF_H
#define MK_DUDA_FCONF_H

/* config constants */
#define DUDA_CONFIG_STR    MK_CONFIG_VAL_STR
#define DUDA_CONFIG_NUM    MK_CONFIG_VAL_NUM
#define DUDA_CONFIG_BOOL   MK_CONFIG_VAL_BOOL
#define DUDA_CONFIG_LIST   MK_CONFIG_VAL_LIST

#define duda_string_line   mk_string_line

/* Remap configuration structures from Monkey core */
struct duda_config
{
    int created;
    char *file;

    /* list of sections */
    struct mk_list sections;
};

struct duda_config_section
{
    char *name;

    struct mk_list entries;
    struct mk_list _head;
};

struct duda_config_entry
{
    char *key;
    char *val;

    struct mk_list _head;
};


/* Object API */

struct duda_api_fconf {
    #define get_path() _get_path(self)
    const char *(*_get_path) (struct web_service *);

    #define set_path(dir)  _set_path(self, dir)
    int (*_set_path) (struct web_service *, const char *);

    #define read_file(f) _read_file(self, f)
    char *(*read_file) (const char *);

    /* --- specific handlers for struct duda_config --- */
    #define read_conf(f)   _read_conf(self, f)
    struct duda_config *(*_read_conf) (struct web_service *, const char *);

    void (*free_conf) (struct duda_config *);
    struct duda_config_section *(*section_get) (struct duda_config *,
                                                const char *);
    void *(*section_key) (struct duda_config_section *, char *, int);
};

struct duda_api_fconf *duda_fconf_object();

#endif
