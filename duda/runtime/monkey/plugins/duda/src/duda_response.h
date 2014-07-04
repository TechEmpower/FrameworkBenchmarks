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

#ifndef DUDA_API_RESPONSE_H
#define DUDA_API_RESPONSE_H

/* RESPONSE object: response->x() */
struct duda_api_response {

    int (*send_headers)  (duda_request_t *);
    int (*headers_off)   (duda_request_t *);
    int (*http_status)   (duda_request_t *, int);
    int (*http_header)   (duda_request_t *, char *);
    int (*http_header_n) (duda_request_t *, char *, int);
    int (*http_content_length) (duda_request_t *, long);
    int (*http_content_type) (duda_request_t *, char *);
    int (*print)  (duda_request_t *, char *, int);
    int (*printf) (duda_request_t *, const char *, ...);
    int (*sendfile)       (duda_request_t *, char *);
    int (*sendfile_range) (duda_request_t *, char *, off_t offset, size_t count);

    int (*wait) (duda_request_t *);
    int (*cont) (duda_request_t *);

    #define end(dr, cb) _end(dr, cb); return;
    int (*_end) (duda_request_t *, void (*end_callback) ());

    #define finalize(dr, cb)  _end(dr, cb);
    int (*_finalize) (duda_request_t *, void (*end_callback) ());

    int (*flush)(duda_request_t *dr);

};

int duda_response_send_headers(duda_request_t *dr);
int duda_response_http_status(duda_request_t *dr, int status);
int duda_response_http_header(duda_request_t *dr, char *row);
int duda_response_http_header_n(duda_request_t *dr, char *row, int len);
int duda_response_http_content_length(duda_request_t *dr, long length);
int duda_response_print(duda_request_t *dr, char *raw, int len);
int duda_response_printf(duda_request_t *dr, const char *format, ...);
int duda_response_sendfile(duda_request_t *dr, char *path);
int duda_response_continue(duda_request_t *dr);
int duda_response_wait(duda_request_t *dr);
int duda_response_end(duda_request_t *dr, void (*end_cb) (duda_request_t *));
int duda_response_flush(duda_request_t *dr);

struct duda_api_response *duda_response_object();

#endif
