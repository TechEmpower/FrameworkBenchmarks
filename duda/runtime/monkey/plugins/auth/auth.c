/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2013, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301  USA
 */

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "MKPlugin.h"

#include "auth.h"
#include "conf.h"
#include "sha1.h"
#include "base64.h"

MONKEY_PLUGIN("auth",              /* shortname */
              "Authentication",    /* name */
              VERSION,             /* version */
              MK_PLUGIN_STAGE_30); /* hooks */

static int mk_auth_validate_user(struct users_file *users,
                                 const char *credentials, unsigned int len)
{
    int sep;
    size_t auth_len;
    unsigned char *decoded = NULL;
    unsigned char digest[SHA1_DIGEST_LEN];
    struct mk_list *head;
    struct user *entry;

    SHA_CTX sha; /* defined in sha1/sha1.h */

    /* Validate value length */
    if (len <= auth_header_basic.len + 1) {
        return -1;
    }

    /* Validate 'basic' credential type */
    if (strncmp(credentials, auth_header_basic.data,
                auth_header_basic.len) != 0) {
        return -1;
    }

    /* Decode credentials: incoming credentials comes in base64 encode */
    decoded = base64_decode((unsigned char *) credentials + auth_header_basic.len,
                            len - auth_header_basic.len,
                            &auth_len);
    if (decoded == NULL) {
        PLUGIN_TRACE("Failed to decode credentials.");
        goto error;
    }

    if (auth_len <= 3) {
        goto error;
    }

    sep = mk_api->str_search_n((char *) decoded, ":", 1, auth_len);
    if (sep == -1 || sep == 0  || (unsigned int) sep == auth_len - 1) {
        goto error;
    }

    /* Get SHA1 hash */
    SHA1_Init(&sha);
    SHA1_Update(&sha, (unsigned char *) decoded + sep + 1, auth_len - (sep + 1));
    SHA1_Final(digest, &sha);

    mk_list_foreach(head, &users->_users) {
        entry = mk_list_entry(head, struct user, _head);
        /* match user */
        if (strlen(entry->user) != (unsigned int) sep) {
            continue;
        }
        if (strncmp(entry->user, (char *) decoded, sep) != 0) {
            continue;
        }

        PLUGIN_TRACE("User match '%s'", entry->user);

        /* match password */
        if (memcmp(entry->passwd_decoded, digest, SHA1_DIGEST_LEN) == 0) {
            PLUGIN_TRACE("User '%s' matched password", entry->user);
            mk_api->mem_free(decoded);
            return 0;
        }
        PLUGIN_TRACE("Invalid password");
        break;
    }

    error:
    if (decoded) {
        mk_api->mem_free(decoded);
    }
    return -1;
}

int _mkp_init(struct plugin_api **api, char *confdir)
{
    (void) confdir;

    mk_api = *api;

    /* Init and load global users list */
    mk_list_init(&vhosts_list);
    mk_list_init(&users_file_list);
    mk_auth_conf_init_users_list();

    /* Set HTTP headers key */
    auth_header_request.data = MK_AUTH_HEADER_REQUEST;
    auth_header_request.len  = sizeof(MK_AUTH_HEADER_REQUEST) - 1;

    auth_header_basic.data = MK_AUTH_HEADER_BASIC;
    auth_header_basic.len  = sizeof(MK_AUTH_HEADER_BASIC) - 1;

    return 0;
}

void _mkp_exit()
{
}

void _mkp_core_thctx()
{
    char *user;

    /* Init thread buffer for given credentials */
    user = mk_api->mem_alloc(MK_AUTH_CREDENTIALS_LEN - 1);
    pthread_setspecific(_mkp_data, (void *) user);
}

/* Object handler */
int _mkp_stage_30(struct plugin *plugin,
                  struct client_session *cs,
                  struct session_request *sr)
{
    int val;
    short int is_restricted = MK_FALSE;
    mk_pointer res;
    (void) plugin;
    struct mk_list *vh_head;
    struct mk_list *loc_head;
    struct vhost *vh_entry = NULL;
    struct location *loc_entry;

    PLUGIN_TRACE("[FD %i] Handler received request");

    /* Match auth_vhost with global vhost */
    mk_list_foreach(vh_head, &vhosts_list) {
        vh_entry = mk_list_entry(vh_head, struct vhost, _head);
        if (vh_entry->host == sr->host_conf) {
            PLUGIN_TRACE("[FD %i] host matched %s",
                         cs->socket,
                         vh_entry->host->host_signature);
            break;
        }
    }

    /* Check vhost locations */
    mk_list_foreach(loc_head, &vh_entry->locations) {
        loc_entry = mk_list_entry(loc_head, struct location, _head);
        if (sr->uri_processed.len < loc_entry->path.len) {
            continue;
        }
        if (strncmp(sr->uri_processed.data,
                    loc_entry->path.data, loc_entry->path.len) == 0) {
            is_restricted = MK_TRUE;
            PLUGIN_TRACE("[FD %i] Location matched %s",
                         cs->socket,
                         loc_entry->path.data);
            break;
        }
    }

    /* For non-restricted location do not take any action, just returns */
    if (is_restricted == MK_FALSE) {
        return MK_PLUGIN_RET_NOT_ME;
    }

    /* Check authorization header */
    res = mk_api->header_get(&sr->headers_toc,
                             auth_header_request.data,
                             auth_header_request.len);

    if (res.data && res.len > 0) {
        /* Validate user */
        val = mk_auth_validate_user(loc_entry->users, res.data, res.len);
        if (val == 0) {
            /* user validated, success */
            PLUGIN_TRACE("[FD %i] user validated!", cs->socket);
            return MK_PLUGIN_RET_NOT_ME;
        }
    }

    /* Restricted access: requires auth */
    PLUGIN_TRACE("[FD %i] unauthorized user, credentials required",
                 cs->socket);

    sr->headers.content_length = 0;
    mk_api->header_set_http_status(sr, MK_CLIENT_UNAUTH);
    mk_api->header_add(sr,
                       loc_entry->auth_http_header.data,
                       loc_entry->auth_http_header.len);
    mk_api->header_send(cs->socket, cs, sr);

    return MK_PLUGIN_RET_END;
}
