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

#ifndef DUDA_QUEUE_H
#define DUDA_QUEUE_H

#include "duda.h"

#define DUDA_QTYPE_ERROR        -1
#define DUDA_QTYPE_BODY_BUFFER   1
#define DUDA_QTYPE_SENDFILE      2

/* Queue item status */
#define DUDA_QSTATUS_ACTIVE      1
#define DUDA_QSTATUS_INACTIVE    0

struct duda_queue_item {
    short int type;        /* item type */
    short int status;      /* item status */
    void *data;            /* the data it self */

    struct mk_list _head;  /* link to the queue list */
};

struct duda_queue_item *duda_queue_item_new(short int type);
int duda_queue_add(struct duda_queue_item *item, struct mk_list *queue);
struct duda_queue_item *duda_queue_last(struct mk_list *queue);
unsigned long duda_queue_length(struct mk_list *queue);
int duda_queue_flush(duda_request_t *dr);
int duda_queue_free(struct mk_list *queue);

int duda_queue_event_write_callback(int sockfd);
int duda_queue_event_is_registered_write(duda_request_t *dr);
int duda_queue_event_register_write(duda_request_t *dr);
int duda_queue_event_unregister_write(duda_request_t *dr);

#endif
