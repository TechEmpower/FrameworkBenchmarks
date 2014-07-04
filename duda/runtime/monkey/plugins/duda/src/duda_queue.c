/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  (C) 2012-2013, Eduardo Silva P. <edsiper@gmail.com>
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
#include "duda_event.h"
#include "duda_queue.h"
#include "duda_sendfile.h"
#include "duda_body_buffer.h"

struct duda_queue_item *duda_queue_item_new(short int type)
{
    struct duda_queue_item *item;

    item = mk_api->mem_alloc(sizeof(struct duda_queue_item));
    if (!item) {
        return NULL;
    }

    item->type   = type;
    item->status = DUDA_QSTATUS_ACTIVE;

    return item;
}

int duda_queue_add(struct duda_queue_item *item, struct mk_list *queue)
{
    mk_list_add(&item->_head, queue);
    return 0;
}

struct duda_queue_item *duda_queue_last(struct mk_list *queue)
{
    if (mk_list_is_empty(queue) == 0) {
        return NULL;
    }

    return mk_list_entry_last(queue, struct duda_queue_item, _head);
}

unsigned long duda_queue_length(struct mk_list *queue)
{
    uint64_t length = 0;
    struct mk_list *head;
    struct duda_queue_item *entry;
    struct duda_sendfile *entry_sf;
    struct duda_body_buffer *entry_bb;

    mk_list_foreach(head, queue) {
        entry = mk_list_entry(head, struct duda_queue_item, _head);
        if (entry->type == DUDA_QTYPE_BODY_BUFFER) {
            entry_bb = (struct duda_body_buffer *) entry->data;
            length += entry_bb->buf->total_len;
        }
        else if (entry->type == DUDA_QTYPE_SENDFILE) {
            entry_sf = (struct duda_sendfile *) entry->data;
            length += entry_sf->pending_bytes;
        }
    }

    return length;
}

int duda_queue_flush(duda_request_t *dr)
{
    int ret = -1;
    int socket = dr->cs->socket;
    short int is_registered;
    unsigned long queue_len=0;
    struct mk_list *head;
    struct duda_queue_item *item;

    mk_list_foreach(head, &dr->queue_out) {
        item = mk_list_entry(head, struct duda_queue_item, _head);
        if (item->status == DUDA_QSTATUS_INACTIVE) {
            continue;
        }

        switch (item->type) {
        case DUDA_QTYPE_BODY_BUFFER:
            ret = duda_body_buffer_flush(socket, item->data);
            break;
        case DUDA_QTYPE_SENDFILE:
            ret = duda_sendfile_flush(socket, item->data);
            break;
        }

        if (ret == 0) {
            item->status = DUDA_QSTATUS_INACTIVE;
        }

        is_registered = duda_queue_event_is_registered_write(dr);
        queue_len = duda_queue_length(&dr->queue_out);

        if (queue_len > 0 && is_registered == MK_FALSE) {
            duda_queue_event_register_write(dr);
        }
        else if (queue_len == 0 && is_registered == MK_TRUE) {
            duda_queue_event_unregister_write(dr);
        }
        break;
    }

    return queue_len;
}

int duda_queue_free(struct mk_list *queue)
{
    struct mk_list *head, *temp;
    struct duda_queue_item *item;
    struct duda_sendfile *sf;
    struct duda_body_buffer *bb;

    mk_list_foreach_safe(head, temp, queue) {
        item = mk_list_entry(head, struct duda_queue_item, _head);
        if (item->type == DUDA_QTYPE_BODY_BUFFER) {
            bb = (struct duda_body_buffer *) item->data;
            mk_api->iov_free(bb->buf);
            mk_api->mem_free(bb);
        }
        else if(item->type == DUDA_QTYPE_SENDFILE) {
            sf = (struct duda_sendfile *) item->data;
            close(sf->fd);
            mk_api->mem_free(sf);
        }
        item->data = NULL;
        mk_list_del(head);
        mk_api->mem_free(item);
    }

    return 0;
}


int duda_queue_event_register_write(duda_request_t *dr)
{
    struct mk_list *list;

    list = pthread_getspecific(duda_global_events_write);
    if (!list) {
        return -1;
    }

    mk_list_add(&dr->_head_events_write, list);
    return 0;
}

int duda_queue_event_unregister_write(duda_request_t *dr)
{
    struct mk_list *list, *head, *temp;
    duda_request_t *entry;

    list = pthread_getspecific(duda_global_events_write);
    mk_list_foreach_safe(head, temp, list) {
        entry = mk_list_entry(head, duda_request_t, _head_events_write);
        if (entry == dr) {
            mk_list_del(&entry->_head_events_write);
            pthread_setspecific(duda_global_events_write, list);
            return 0;
        }
    }

    return -1;
}

int duda_queue_event_is_registered_write(duda_request_t *dr)
{
    struct mk_list *list;
    struct mk_list *head;
    duda_request_t *entry;

    list = pthread_getspecific(duda_global_events_write);
    mk_list_foreach(head, list) {
        entry = mk_list_entry(head, duda_request_t, _head_events_write);
        if (entry == dr) {
            return MK_TRUE;
        }
    }

    return MK_FALSE;
}

int duda_queue_event_write_callback(int sockfd)
{
    int ret = MK_PLUGIN_RET_CONTINUE;
    struct mk_list *list, *temp, *head;
    duda_request_t *entry;

    list = pthread_getspecific(duda_global_events_write);
    mk_list_foreach_safe(head, temp, list) {
        entry = mk_list_entry(head, duda_request_t, _head_events_write);
        if (entry->cs->socket == sockfd) {
            ret = duda_queue_flush(entry);
            if (ret > 0) {
                return MK_PLUGIN_RET_EVENT_OWNED;
            }
            if (duda_service_end(entry) == -1) {
                return MK_PLUGIN_RET_EVENT_CLOSE;
            }
            else {
                return MK_PLUGIN_RET_EVENT_OWNED;
            }

            break;
        }
    }

    return MK_PLUGIN_RET_EVENT_CONTINUE;
}
