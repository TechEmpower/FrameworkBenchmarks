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

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <fcntl.h>
#include <dirent.h>

#include "MKPlugin.h"
#include "duda_session.h"
#include "duda.h"
#include "webservice.h"
#include "duda_conf.h"

/*
 * @OBJ_NAME: session
 * @OBJ_MENU: Sessions
 * @OBJ_DESC: The sessions object provides a set of methods to handle persistent data
 * associated to a set of HTTP request coming from the same source. Sessions can be used
 * to store any kind of information in memory.
 *
 * @OBJ_COMM: Duda sessions are stored into /dev/shm, yes, we know that is not expected as
 * the main purpose of /dev/shm is for process intercommunication and we are breaking
 * the rule. Mount a filesystem before launch the service is an extra step, do that
 * inside Duda will generate permission issues.. so ?, we use /dev/shm.
 */

struct duda_api_session *duda_session_object()
{
    struct duda_api_session *s;

    s = mk_api->mem_alloc(sizeof(struct duda_api_session));
    s->init    = duda_session_init;
    s->create  = duda_session_create;
    s->destroy = duda_session_destroy;
    s->get     = duda_session_get;
    s->isset   = duda_session_isset;

    return s;
}

int _duda_session_create_store(const char *path)
{
    int ret;

    ret = mkdir(path, SESSION_DEFAULT_PERM);
    if (ret != 0) {
        mk_err("duda_session: could not create SESSION_STORE_PATH '%s'", SESSION_STORE_PATH);
        return -1;
    }

    return 0;
}

/*
 * @METHOD_NAME: init
 * @METHOD_DESC: Initialize the sessions object for the web service in question. This function
 * must be invoked from duda_main().
 * @METHOD_PROTO: int init(char *store_name)
 * @METHOD_PARAM: store_name directory name to identify the session files under /dev/shm/duda_sessions/
 * @METHOD_RETURN: Upon successful completion it returns 0. On error it returns NULL.
 */
int duda_session_init(char *store_name)
{
    int ret;
    char *path = NULL;
    unsigned long len;
    struct file_info finfo;

    session_store_path = NULL;

    /*
     * the 'shm' mount point can be located now on /dev/ or in the new
     * interface /run/ depending of the Linux distribution. We need to
     * check which one is being used by the system.
     */
    ret = mk_api->file_get_info("/dev/shm", &finfo);
    if (ret == 0) {
        session_store_path = SESSION_STORE_PATH_DEV;
    }
    else {
        ret = mk_api->file_get_info("/run/shm", &finfo);
        if (ret == 0) {
            session_store_path = SESSION_STORE_PATH_RUN;
        }
    }

    ret = mk_api->file_get_info(session_store_path, &finfo);
    if (ret != 0) {
        if (_duda_session_create_store(session_store_path) != 0) {
            return -1;
        }
    }

    mk_api->str_build(&path, &len, "%s/%s", session_store_path, store_name);
    ret = mk_api->file_get_info(path, &finfo);
    if (ret != 0) {
        if (_duda_session_create_store(path) != 0) {
            return -1;
        }
    }
    
    session_store_path = path;

    return 0;
}


static inline int _rand(int entropy)
{
    struct timeval tm;

    gettimeofday(&tm, NULL);
    srand(tm.tv_usec + entropy);

    return rand();
}

/*
 * @METHOD_NAME: create
 * @METHOD_DESC: It creates a new session for a given request.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: name specifies the session name
 * @METHOD_PARAM: value the value that will be stored in the session
 * @METHOD_PARAM: expires defines the expiration time in unix time seconds
 * @METHOD_RETURN: Upon successful completion it returns 0. On error it returns NULL.
 */
int duda_session_create(duda_request_t *dr, char *name, char *value, int expires)
{
    /* FIXME: It must check for duplicates */
    int n;
    int fd;
    int len;
    long e;
    char *uuid;
    char session[SESSION_UUID_SIZE];

    //struct web_service *ws = dr->ws_root;

    /*
     * generate some random value, lets give some entropy and
     * then generate the UUID to send the proper Cookie to the
     * client.
     */
    e = ((long) &dr) + ((long) &dr->cs) + (dr->cs->socket);
    uuid = mk_api->mem_alloc(SESSION_UUID_SIZE);
    if (!uuid) {
        mk_warn("duda_session: could not allocate space for UUID");
        return -1;
    }

    len = snprintf(uuid, SESSION_UUID_SIZE, "%x-%x",
                   _rand(e), _rand(e));
    duda_cookie_set(dr, "DUDA_SESSION", 12, uuid, len, expires);

    /* session format: expire_time.name.UUID.duda_session */
    snprintf(session, SESSION_UUID_SIZE, "%s/%s.%s.%d",
             session_store_path, name, uuid, expires);
    fd = open(session, O_CREAT | O_WRONLY, 0600);
    if (fd == -1) {
        perror("open");
        mk_err("duda_session: could not create session file");
        return -1;
    }

    n = write(fd, value, strlen(value));
    close(fd);

    if (n == -1) {
        mk_err("duda_session: could not write to session file");
        return -1;
    }

    return 0;
}

int _duda_session_get_path(duda_request_t *dr, char *name, char **buffer, int buf_size)
{
    int ret;
    int len;
    int buf_len;
    char buf[SESSION_UUID_SIZE];
    char *session_val;
    DIR *dir;
    struct dirent *ent;

    /* Get UUID for the specified key */
    ret = duda_cookie_get(dr, SESSION_KEY, &session_val, &len);
    if (ret == -1) {
        return -1;
    }

    /* Open store path */
    if (!(dir = opendir(session_store_path))) {
        return -1;
    }

    /* Compose possible session file name */
    memset(buf, '\0', sizeof(buf));
    ret = snprintf(buf, SESSION_UUID_SIZE, "%s.", name);
    strncpy(buf + ret, session_val, len);
    buf_len = ret + len;
    buf[buf_len++] = '.';
    buf[buf_len  ] = '\0';

    /* Go into session files */
    while ((ent = readdir(dir)) != NULL) {
        if ((ent->d_name[0] == '.') && (strcmp(ent->d_name, "..") != 0)) {
            continue;
        }

        /* Look just for files */
        if (ent->d_type != DT_REG) {
            continue;
        }

        /* try to match the file name */
        if (strncmp(ent->d_name, buf, buf_len) == 0) {
            snprintf(*buffer, buf_size, "%s/%s", session_store_path, ent->d_name);
            closedir(dir);
            return 0;
        }
    }

    closedir(dir);
    return -1;
}

/*
 * @METHOD_NAME: destroy
 * @METHOD_DESC: It destroy a session associated to a request context
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: name specifies the session name
 * @METHOD_RETURN: Upon successful completion it returns 0. On error it returns NULL.
 */
int duda_session_destroy(duda_request_t *dr, char *name)
{
    int ret;
    char *buf = mk_api->mem_alloc(SESSION_UUID_SIZE);

    /* Get the absolute path for the session file */
    ret = _duda_session_get_path(dr, name, &buf, SESSION_UUID_SIZE);
    if (ret == 0) {
        unlink(buf);
    }

    mk_api->mem_free(buf);

    /* Now lets make the client cookie expire */
    duda_cookie_destroy(dr, SESSION_KEY, sizeof(SESSION_KEY) - 1);
    return ret;
}

/*
 * @METHOD_NAME: get
 * @METHOD_DESC: Retrieve in a new memory buffer the value of the stored session
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: name specifies the session name
 * @METHOD_RETURN: Upon successful completion it returns the new buffer with the session
 * value. On error it returns NULL.
 */
void *duda_session_get(duda_request_t *dr, char *name)
{
    int ret;
    char *buf = mk_api->mem_alloc(SESSION_UUID_SIZE);
    char *raw;

    /* We need to catch the right UUID for the session in question */
    ret = _duda_session_get_path(dr, name, &buf, SESSION_UUID_SIZE);
    if (ret == -1) {
        return NULL;
    }

    raw = mk_api->file_to_buffer(buf);
    mk_api->mem_free(buf);
    return raw;
}

/*
 * @METHOD_NAME: isset
 * @METHOD_DESC: Check if a session exist for the given context and session name
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: name specifies the session name
 * @METHOD_RETURN: If the session is set it returns 0. On error or if the session
 * could not be found it returns -1.
 */
int duda_session_isset(duda_request_t *dr, char *name)
{
    int ret;
    int len;
    int buf_len;
    char buf[SESSION_UUID_SIZE];
    char *session_val;
    DIR *dir;
    struct dirent *ent;

    /* Get UUID for the specified key */
    ret = duda_cookie_get(dr, SESSION_KEY, &session_val, &len);
    if (ret == -1) {
        return -1;
    }

    /* Open store path */
    if (!(dir = opendir(session_store_path))) {
        return -1;
    }

    /* Compose possible session file name */
    memset(buf, '\0', sizeof(buf));
    ret = snprintf(buf, SESSION_UUID_SIZE, "%s.", name);
    strncpy(buf + ret, session_val, len);
    buf_len = ret + len;
    buf[buf_len++] = '.';
    buf[buf_len  ] = '\0';

    /* Go into session files */
    while ((ent = readdir(dir)) != NULL) {
        if ((ent->d_name[0] == '.') && (strcmp(ent->d_name, "..") != 0)) {
            continue;
        }

        /* Look just for files */
        if (ent->d_type != DT_REG) {
            continue;
        }

        /* try to match the file name */
        if (strncmp(ent->d_name, buf, buf_len) == 0) {
            closedir(dir);
            return 0;
        }
    }

    closedir(dir);
    return -1;
}
