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

#ifndef DUDA_BODY_BUFFER_H
#define DUDA_BODY_BUFFER_H

/*
 * The response body holds an IOV array struct of BODY_BUFFER_SIZE,
 * when the limit is reached, the pointer is reallocated adding a new chunk
 */
#define BODY_BUFFER_SIZE  8


struct duda_body_buffer {
    struct mk_iov *buf;
    unsigned short int size;
    unsigned long int sent;
};

struct duda_body_buffer *duda_body_buffer_new();
int duda_body_buffer_expand(struct duda_body_buffer *bb);
int duda_body_buffer_flush(int sock, struct duda_body_buffer *bb);

#endif
