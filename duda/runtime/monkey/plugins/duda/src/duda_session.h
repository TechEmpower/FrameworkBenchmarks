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

#ifndef DUDA_SESSION_H
#define DUDA_SESSION_H

#include "duda.h"

#define SESSION_STORE_PATH_DEV  "/dev/shm/duda_sessions"
#define SESSION_STORE_PATH_RUN  "/run/shm/duda_sessions"

#define SESSION_DEFAULT_PERM    0700
#define SESSION_UUID_SIZE       128  /* 128 bytes */
#define SESSION_KEY             "DUDA_SESSION"

char *session_store_path;

struct mk_list session_list;

struct duda_api_session {
    int (*init)     (char *);
    int (*create)   (duda_request_t *, char *, char *, int);
    int (*destroy)  (duda_request_t *, char *);
    void *(*get)    (duda_request_t *, char *);
    int (*isset)    (duda_request_t *, char *);
};

struct duda_api_session *duda_session_object();
int duda_session_init(char *store_name);
int duda_session_create(duda_request_t *dr, char *name, char *value, int expires);
int duda_session_destroy(duda_request_t *dr, char *name);
void *duda_session_get(duda_request_t *dr, char *name);
int duda_session_isset(duda_request_t *dr, char *name);

#endif
