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

#ifndef DUDA_LOG_H
#define DUDA_LOG_H

#include "duda_conf.h"
#include "duda.h"
#include "duda_global.h"
#include "duda_objects.h"
#include "mk_memory.h"

typedef struct {
    int enabled;
    duda_global_t global_key;
    char *name;
} duda_logger_t;

/*
 * A log context that is used to distribute the logs messages to the
 * log pipe and worker
 */
typedef struct {
    int   pipe_fd[2];
    char *name;
    char *log_path;
    duda_logger_t *key;
    struct web_service *ws;
    struct mk_list _head;
} duda_logger_context_t;

pthread_mutex_t duda_logger_mutex;
pthread_key_t duda_logger_fmt_cache;

static inline void *_duda_logger_cb_create(void *data)
{
    duda_logger_context_t *ctx;
    duda_logger_context_t *info = (duda_logger_context_t *) data;

    mk_bug(!data);

    pthread_mutex_lock(&duda_logger_mutex);

    ctx = malloc(sizeof(duda_logger_context_t));
    ctx->pipe_fd[0] = info->pipe_fd[0];
    ctx->pipe_fd[1] = info->pipe_fd[1];
    ctx->name = strdup(info->name);
    ctx->key  = info->key;

    mk_list_add(&ctx->_head, &duda_logger_worker_list);
    pthread_mutex_unlock(&duda_logger_mutex);

    return ctx;
}

static inline int duda_logger_create(duda_logger_t *log, char *name)
{
    unsigned long len;
    duda_logger_context_t *ctx;

    /*
     * create a struct to hold the data for the callback when the threads
     * start creating their loggers scope
     */
    ctx = malloc(sizeof(duda_logger_context_t));

    /* fill the fields */
    ctx->name     = name;
    ctx->log_path = NULL;
    monkey->str_build(&ctx->log_path, &len, "%s/%s", self->logdir.data, name);
    ctx->ws       = self;
    ctx->key      = log;
    if (pipe(ctx->pipe_fd) == -1) {
        perror("pipe");
        exit(EXIT_FAILURE);
    }

    /*
     * we link this data to a main list, so from Duda context we can query
     * all Loggers created
     */
    mk_list_add(&ctx->_head, &duda_logger_main_list);

    memset(log, 0, sizeof(duda_logger_t));
    log->enabled = MK_TRUE;
    duda_global_init(&log->global_key, _duda_logger_cb_create, (void *) ctx);
    return 0;
}

struct duda_api_logger {
    int (*print) (duda_logger_t *, char *, ...);
};

int duda_logger_init();
struct duda_api_logger *duda_logger_object();

#endif
