/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012, Sourabh Chandak <sourabh3934@gmail.com>
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
#include <stdlib.h>

#include "redis.h"

int redis_read(int fd, struct duda_request *dr)
{
    (void) *dr;

    printf("[FD %i] Redis Handler / read\n", fd);
    struct mk_list *list_redis_fd,*head;
    duda_redis_t *dr_entry;
    redisAsyncContext *rc=NULL;
    list_redis_fd = pthread_getspecific(redis_key);

    mk_list_foreach(head, list_redis_fd) {
        dr_entry = mk_list_entry(head, duda_redis_t, _head_redis_fd);
        if(dr_entry->rc->c.fd == fd){
            rc = dr_entry->rc;
            break;
        }
    }
    if(rc == NULL)
        printf("Error\n");
    else
        redisAsyncHandleRead(rc);

    return DUDA_EVENT_OWNED;
}

int redis_write(int fd, struct duda_request *dr)
{
    (void) *dr;

    printf("[FD %i] Redis Handler / write\n", fd);
    struct mk_list *list_redis_fd,*head;
    duda_redis_t *dr_entry;
    redisAsyncContext *rc=NULL;
    list_redis_fd = pthread_getspecific(redis_key);

    mk_list_foreach(head, list_redis_fd) {
        dr_entry = mk_list_entry(head, duda_redis_t, _head_redis_fd);
        if(dr_entry->rc->c.fd == fd){
            rc = dr_entry->rc;
            break;
        }
    }
    if(rc == NULL)
        printf("Error\n");
    else
        redisAsyncHandleWrite(rc);
    return 1;
}

int redis_error(int fd, struct duda_request *dr)
{
    (void) *dr;

    printf("[FD %i] Redis Handler / error\n", fd);
    struct mk_list *list_redis_fd,*head;
    duda_redis_t *dr_entry;
    redisAsyncContext *rc=NULL;
    list_redis_fd = pthread_getspecific(redis_key);

    mk_list_foreach(head, list_redis_fd) {
        dr_entry = mk_list_entry(head, duda_redis_t, _head_redis_fd);
        if(dr_entry->rc->c.fd == fd){
            rc = dr_entry->rc;
            break;
        }
    }
    if(rc == NULL)
        printf("Error\n");
    else
        redisDel(rc);
    return 1;
}

int redis_close(int fd, struct duda_request *dr)
{
    (void) *dr;

    printf("[FD %i] Redis Handler / close\n", fd);
    struct mk_list *list_redis_fd,*head, *tmp;
    duda_redis_t *dr_entry;
    list_redis_fd = pthread_getspecific(redis_key);

    mk_list_foreach_safe(head, tmp, list_redis_fd) {
        dr_entry = mk_list_entry(head, duda_redis_t, _head_redis_fd);
        if(dr_entry->rc->c.fd == fd){
            mk_list_del(&dr_entry->_head_redis_fd);
            pthread_setspecific(redis_key, (void *) list_redis_fd);
            break;
        }
    }
    return 1;
}

int redis_timeout(int fd, struct duda_request *dr)
{
    (void) *dr;

    printf("[FD %i] Redis Handler / timeout\n", fd);

    return 1;

}

redisAsyncContext * redis_connect(const char *ip, int port,
                                  duda_request_t *dr_web)
{
    struct mk_list *list_redis_fd;
    duda_redis_t *dr;
    redisAsyncContext *c = redisAsyncConnect(ip, port);
    if (c->err || c == NULL) {
        printf("REDIS: Can't connect: %s\n", c->errstr);
        exit(EXIT_FAILURE);
    }
    dr = monkey->mem_alloc(sizeof(duda_redis_t));
    dr->rc = c;
    dr->dr = dr_web;
    list_redis_fd = pthread_getspecific(redis_key);
    if(list_redis_fd == NULL)
    {
        list_redis_fd = malloc(sizeof(struct mk_list));
        mk_list_init(list_redis_fd);
        pthread_setspecific(redis_key, (void *) list_redis_fd);
    }

    mk_list_add(&dr->_head_redis_fd, list_redis_fd);
    return c;
}

void redis_disconnect(redisAsyncContext *rc)
{
    redis_free(rc);
    redisAsyncDisconnect(rc);
}

void redis_free(redisAsyncContext *rc)
{
    event->delete(rc->c.fd);
}

int redis_attach(redisAsyncContext *ac, duda_request_t *dr)
{
    event->add(ac->c.fd, dr, DUDA_EVENT_RW, DUDA_EVENT_LEVEL_TRIGGERED,
               redis_read, redis_write, redis_error, redis_close, redis_timeout);

    return REDIS_OK;

}

int redis_init()
{
    pthread_key_create(&redis_key, NULL);

    return 1;
}

duda_request_t * redis_request_map(const redisAsyncContext *rc)
{
    struct mk_list *list_redis_fd,*head,*tmp;
    duda_request_t *dr;
    duda_redis_t *dr_entry;
    list_redis_fd = pthread_getspecific(redis_key);

    mk_list_foreach_safe(head, tmp, list_redis_fd) {
        dr_entry = mk_list_entry(head, duda_redis_t, _head_redis_fd);
        if(dr_entry->rc == rc) {
            dr = dr_entry->dr;
            mk_list_del(&dr_entry->_head_redis_fd);
            free(dr_entry);
            return dr;
        }
    }
    return NULL;
}
