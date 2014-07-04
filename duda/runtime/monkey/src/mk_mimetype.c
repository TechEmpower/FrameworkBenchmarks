/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2014, Eduardo Silva P. <edsiper@gmail.com>
 *  Copyright (C) 2011 Davidlohr Bueso <dave@gnu.org>
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU Lesser General Public  License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "monkey.h"
#include "mk_mimetype.h"
#include "mk_memory.h"
#include "mk_string.h"
#include "mk_utils.h"
#include "mk_config.h"
#include "mk_request.h"
#include "mk_list.h"
#include "mk_macros.h"

struct mimetype *mimetype_default;
struct mimetype *mimearr = NULL; /* array of the mime types */

/* Match mime type for requested resource */
inline struct mimetype *mk_mimetype_lookup(char *name)
{
    int cmp;
  	struct rb_node *node = mimetype_rb_head.rb_node;

  	while (node) {
  		struct mimetype *entry = container_of(node, struct mimetype, _rb_head);

        cmp = strcmp(name, entry->name);
		if (cmp < 0)
  			node = node->rb_left;
		else if (cmp > 0)
  			node = node->rb_right;
		else {
  			return entry;
        }
	}
	return NULL;
}

int mk_mimetype_add(char *name, const char *type)
{
    int cmp;
    int len = strlen(type) + 3;
    char *p;
    struct mimetype *new_mime;
    struct rb_node **new;
    struct rb_node *parent = NULL;

    /* make sure we register the extension in lower case */
    p = name;
    for ( ; *p; ++p) *p = tolower(*p);

    new_mime = mk_mem_malloc_z(sizeof(struct mimetype));
    new_mime->name = mk_string_dup(name);
    new_mime->type.data = mk_mem_malloc(len);
    new_mime->type.len = len - 1;
    strcpy(new_mime->type.data, type);
    strcat(new_mime->type.data, MK_CRLF);
    new_mime->type.data[len-1] = '\0';

    /* Red-Black tree insert routine */
    new = &(mimetype_rb_head.rb_node);

    /* Figure out where to put new node */
    while (*new) {
        struct mimetype *this = container_of(*new, struct mimetype, _rb_head);

        parent = *new;
        cmp = strcmp(new_mime->name, this->name);
        if (cmp < 0) {
            new = &((*new)->rb_left);
        }
        else if (cmp > 0) {
            new = &((*new)->rb_right);
        }
        else {
            return -1;
        }
    }

    /* Add new node and rebalance tree. */
    rb_link_node(&new_mime->_rb_head, parent, new);
    rb_insert_color(&new_mime->_rb_head, &mimetype_rb_head);

    /* Add to linked list head */
    mk_list_add(&new_mime->_head, &mimetype_list);

    return 0;
}

/* Load the two mime arrays into memory */
void mk_mimetype_read_config()
{
    char path[MK_MAX_PATH];
    struct mk_config *cnf;
    struct mk_config_section *section;
    struct mk_config_entry *entry;
    struct mk_list *head;

    /* Initialize the heads */
    mk_list_init(&mimetype_list);
    mimetype_rb_head = RB_ROOT;

    /* Read mime types configuration file */
    snprintf(path, MK_MAX_PATH, "%s/monkey.mime", config->serverconf);
    cnf = mk_config_create(path);
    if (!cnf) {
        exit(EXIT_FAILURE);
    }

    /* Get MimeTypes tag */
    section = mk_config_section_get(cnf, "MIMETYPES");
    if (!section) {
        mk_err("Error: Invalid mime type file");
        exit(EXIT_FAILURE);
    }

    mk_list_foreach(head, &section->entries) {
        entry = mk_list_entry(head, struct mk_config_entry, _head);
        if (!entry->key || !entry->val) {
            continue;
        }

        if (mk_mimetype_add(entry->key, entry->val) != 0) {
            mk_err("Error loading Mime Types");
            exit(EXIT_FAILURE);
        }
    }

    /* Set default mime type */
    mimetype_default = mk_mem_malloc_z(sizeof(struct mimetype));
    mimetype_default->name = MIMETYPE_DEFAULT_TYPE;
    mk_pointer_set(&mimetype_default->type, config->default_mimetype);

    mk_config_free(cnf);
}

struct mimetype *mk_mimetype_find(mk_pointer *filename)
{
    int j, len;

    j = len = filename->len;

    /* looking for extension */
    while (filename->data[j] != '.' && j >= 0) {
        j--;
    }

    if (j <= 0) {
        return NULL;
    }

    return mk_mimetype_lookup(filename->data + j + 1);
}
