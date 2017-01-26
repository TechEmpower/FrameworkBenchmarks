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

#define  _GNU_SOURCE
#include <limits.h>

#include "MKPlugin.h"

#include "duda_event.h"
#include "duda_body_buffer.h"

struct duda_body_buffer *duda_body_buffer_new()
{
    struct duda_body_buffer *bb;

    bb = mk_api->mem_alloc_z(sizeof(struct duda_body_buffer));
    bb->buf = mk_api->iov_create(BODY_BUFFER_SIZE, 0);
    bb->size = BODY_BUFFER_SIZE;
    bb->sent = 0;

    return bb;
}

int duda_body_buffer_expand(struct duda_body_buffer *bb)
{
    int size = bb->buf->size + BODY_BUFFER_SIZE;

    if (mk_api->iov_realloc(bb->buf, size) == -1) {
        return -1;
    }

    bb->size = size;
    return 0;
}

int duda_body_buffer_flush(int sock, struct duda_body_buffer *bb)
{
    int i;
    int count = 0;
    unsigned int bytes_sent, bytes_to;
    int reset_to = -1;
    struct mk_iov *buf = bb->buf;

    /* FIXME: Temporal Check */
    if (mk_unlikely(buf->iov_idx > IOV_MAX)) {
        mk_err("Boddy buffer flush: enqueued data is larger than IOV_MAX (%i)\n",
                IOV_MAX);
        exit(EXIT_FAILURE);
    }

    bytes_sent = mk_api->socket_sendv(sock, buf);
    PLUGIN_TRACE("body_flush: %i/%i", bytes_sent, buf->total_len);

    /*
     * If the call sent less data than total, we must modify the mk_iov struct
     * to mark the buffers already processed and set them with with length = zero,
     * so on the next calls to this function Monkey will skip buffers with bytes
     * length = 0.
     */
    if (bytes_sent < buf->total_len) {
        /* Go around each buffer entry and check where the offset took place */
        for (i = 0; i < buf->iov_idx; i++) {
            if (count + buf->io[i].iov_len == bytes_sent) {
                reset_to = i;
                break;
            }
            else if (bytes_sent < (count + buf->io[i].iov_len)) {
                reset_to = i - 1;
                bytes_to = (bytes_sent - count);
                buf->io[i].iov_base += bytes_to;
                buf->io[i].iov_len   = buf->io[i].iov_len - bytes_to;
                break;
            }
            count += buf->io[i].iov_len;
        }

        /* Reset entries */
        for (i = 0; i <= reset_to; i++) {
            buf->io[i].iov_len = 0;
        }

        buf->total_len -= bytes_sent;

#ifdef TRACE
        PLUGIN_TRACE("new total len: %i (iov_idx=%i)",
                     buf->total_len,
                     buf->iov_idx);
        int j;

        for (j = 0; j < buf->iov_idx; j++) {
            PLUGIN_TRACE("io[%i] = %i", j, buf->io[j].iov_len);
        }
#endif
    }

    /* Successfully end ? */
    if (bytes_sent == buf->total_len) {
        buf->total_len = 0;
        return 0;
    }

    return bytes_sent;
}
