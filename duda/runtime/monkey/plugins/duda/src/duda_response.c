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
#include <stdarg.h>

#include "mk_mimetype.h"

#include "duda.h"
#include "duda_api.h"
#include "duda_queue.h"
#include "duda_event.h"
#include "duda_sendfile.h"
#include "duda_body_buffer.h"
#include "duda_response.h"

/*
 * @OBJ_NAME: response
 * @OBJ_MENU: Response
 * @OBJ_DESC: The response object provides a set of methods to manipulate the
 * response to the HTTP client. It helps to compose response headers as well
 * the body content.
 */


/*
 * @METHOD_NAME: headers_off
 * @METHOD_DESC: It tells Duda I/O Core that no headers will be send as part
 * of the response.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: It always return zero.
 */
int duda_response_headers_off(duda_request_t *dr)
{
    dr->_st_http_headers_off = MK_TRUE;
    return 0;
}

/*
 * @METHOD_NAME: send_headers
 * @METHOD_DESC: Send the HTTP response headers
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_send_headers(duda_request_t *dr)
{
    int r;

    if (dr->_st_http_headers_off == MK_TRUE) {
        dr->_st_http_headers_sent = MK_TRUE;
        return 0;
    }

    if (dr->_st_http_headers_sent == MK_TRUE) {
        return -1;
    }

    if (dr->_st_body_writes > 0) {
        /* FIXME: Console error */
        return -1;
    }

    /* Calculate body length */
    if (dr->_st_http_content_length == -2) {
        dr->sr->headers.content_length = duda_queue_length(&dr->queue_out);
    }
    else if (dr->_st_http_content_length >= 0) {
        dr->sr->headers.content_length = dr->_st_http_content_length;
    }

    r = mk_api->header_send(dr->cs->socket, dr->cs, dr->sr);
    if (r != 0) {
        /* FIXME: Console error */
        return -1;
    }

    /* Change flag status */
    dr->_st_http_headers_sent = MK_TRUE;
    return 0;
}

/*
 * @METHOD_NAME: http_status
 * @METHOD_DESC: It set the HTTP response status code
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: status the HTTP code status
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_http_status(duda_request_t *dr, int status)
{
    mk_api->header_set_http_status(dr->sr, status);
    return 0;
}

/*
 * @METHOD_NAME: http_header
 * @METHOD_DESC: It adds a new HTTP header to the response
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: row fixed string containing the header details, it must not include CRLF
 * or similar break line characters.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_http_header(duda_request_t *dr, char *row)
{
    return mk_api->header_add(dr->sr, row, strlen(row));
}

/*
 * @METHOD_NAME: http_header_n
 * @METHOD_DESC: It adds a new HTTP header to the response but specifying the number of bytes.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: row fixed string containing the header details, it must not include CRLF
 * or similar break line characters.
 * @METHOD_PARAM: len specify the number of bytes of 'row' to set as header.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_http_header_n(duda_request_t *dr, char *row, int len)
{
    return mk_api->header_add(dr->sr, row, len);
}

/*
 * @METHOD_NAME: http_content_length
 * @METHOD_DESC: It sets a fixed content length header value. If you set the value -1 the Content-Length header will not be send.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: length the body length.
 * @METHOD_RETURN: This method always returns zero
 */
int duda_response_http_content_length(duda_request_t *dr, long length)
{
    dr->_st_http_content_length = length;
    return 0;
}

/*
 * @METHOD_NAME: http_content_type
 * @METHOD_DESC: Given a known mime extension, it lookup the mime type associated and
 * compose the HTTP Content-Type header.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: extension the mime extension. e.g: 'jpg'.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_http_content_type(duda_request_t *dr, char *extension)
{
    int len;
    char *header;
    struct mimetype *m;

    m = mk_api->mimetype_lookup(extension);
    if (!m) {
        return -1;
    }

    header = mk_api->mem_alloc(32);
    len = snprintf(header, 32, "Content-Type: %s", m->type.data);
    duda_response_http_header_n(dr, header, len - 2);
    duda_gc_add(dr, header);

    return 0;
}

/* Compose the body_buffer */
static int _print(duda_request_t *dr, char *raw, int len, int free)
{
    int ret;
    struct duda_body_buffer *body_buffer;
    struct duda_queue_item *item;

    item = duda_queue_last(&dr->queue_out);
    if (!item || item->type != DUDA_QTYPE_BODY_BUFFER) {
        body_buffer = duda_body_buffer_new();
        item = duda_queue_item_new(DUDA_QTYPE_BODY_BUFFER);
        item->data = body_buffer;
        duda_queue_add(item, &dr->queue_out);
    }
    else {
        body_buffer = item->data;

    }

    /* perform realloc if body_write() is called more than body_buffer_size */
    if (body_buffer->buf->iov_idx >= body_buffer->size)  {
        ret = duda_body_buffer_expand(body_buffer);
        if (ret == -1) {
            return -1;
        }
    }

    /* Link data */
    if (free == MK_TRUE) {
        mk_api->iov_add_entry(body_buffer->buf, raw, len,
                              dd_iov_none, MK_IOV_FREE_BUF);
    }
    else {
        mk_api->iov_add_entry(body_buffer->buf, raw, len,
                              dd_iov_none, MK_IOV_NOT_FREE_BUF);
    }

    return 0;
}

/*
 * @METHOD_NAME: print
 * @METHOD_DESC: It enqueue a buffer of data to be send to the HTTP client as response body.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: raw Fixed buffer of data to be send to the client
 * @METHOD_PARAM: len Number of bytes of 'raw' to be send.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_print(duda_request_t *dr, char *raw, int len)
{
    return _print(dr, raw, len, MK_FALSE);
}


/*
 * @METHOD_NAME: printf
 * @METHOD_DESC: It format and enqueue a buffer of data to be send to the HTTP client as response body.
 * The buffer allocated is freed internally when the data is flushed completely.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: format Specifies the subsequent arguments to be formatted
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_printf(duda_request_t *dr, const char *format, ...)
{
    int ret;
    int n, size = 128;
    char *p, *np;
    va_list ap;

    if ((p = mk_api->mem_alloc(size)) == NULL) {
        return -1;
    }

    /* Try to print in the allocated space. */
    while (1) {
        va_start(ap, format);
        n = vsnprintf(p, size, format, ap);

        /* If that worked, return the string. */
        if (n > -1 && n < size)
            break;

        size *= 2;  /* twice the old size */
        if ((np = mk_api->mem_realloc(p, size)) == NULL) {
            mk_api->mem_free(p);
            return - 1;
        } else {
            p = np;
        }
    }
    va_end(ap);

    ret = _print(dr, p, n, MK_TRUE);
    if (ret == -1) {
        mk_api->mem_free(p);
    }

    return ret;
}

/*
 * @METHOD_NAME: sendfile
 * @METHOD_DESC: It enqueue a filesystem file to be send to the HTTP client as response body. Multiple
 * files can be enqueued, all of them are send in order.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: path the absolute path of the file to be send.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_sendfile(duda_request_t *dr, char *path)
{
    struct duda_sendfile *sf;
    struct duda_queue_item *item;

    sf = duda_sendfile_new(path, 0, 0);
    if (!sf) {
        return -1;
    }

    item = duda_queue_item_new(DUDA_QTYPE_SENDFILE);
    item->data = sf;
    duda_queue_add(item, &dr->queue_out);

    return 0;
}


/*
 * @METHOD_NAME: sendfile_range
 * @METHOD_DESC: It enqueue a bytes range from a file to be send to the HTTP client as
 * response body. Multiple files can be enqueued, all of them are send in order. This
 * method requires an offset and the total number of bytes to send after that. Both values must be equal or greater than zero.
 * @METHOD_PROTO: int sendfile_range(duda_request_t *dr, char *path, off_t offset, size_t count)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: path the absolute path of the file to be send.
 * @METHOD_PARAM: offset set the offset in bytes.
 * @METHOD_PARAM: count number of bytes to send. Specifying this value as zero, it
 * will try to send the whole remaining bytes availables on the file after the offset.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_sendfile_range(duda_request_t *dr, char *path,
                                 off_t offset, size_t count)
{
    struct duda_sendfile *sf;
    struct duda_queue_item *item;

    sf = duda_sendfile_new(path, offset, count);
    if (!sf) {
        return -1;
    }

    item = duda_queue_item_new(DUDA_QTYPE_SENDFILE);
    item->data = sf;
    duda_queue_add(item, &dr->queue_out);

    return 0;
}


/*
 * @METHOD_NAME: wait
 * @METHOD_DESC: It instruct Duda to put the response in sleep mode. This call is usually
 * needed when the response depends of a third party resource and there is not need to
 * have the response active until some specific event ocurr. The response can be activated
 * later with the continue() method.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_wait(duda_request_t *dr)
{
    /*
     * send the socket to sleep, the behavior is not required as the Monkey 'event
     * states' already have the previous mode and behavior
     */
    return mk_api->event_socket_change_mode(dr->cs->socket, DUDA_EVENT_SLEEP, -1);
}

/*
 * @METHOD_NAME: cont
 * @METHOD_DESC: It restore and continue the context of a previous wait() call, all events
 * are restored.
 * @METHOD_PROTO: int cont(duda_request_t *dr)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_continue(duda_request_t *dr)
{
    return mk_api->event_socket_change_mode(dr->cs->socket, DUDA_EVENT_WAKEUP, -1);
}

/*
 * @METHOD_NAME: end
 * @METHOD_DESC: It indicate that the full response for the request has been ended. No
 * extra calls will take place after invoke this method as it contains an implicit return
 * for the active callback.
 *
 * Internally, this function send the HTTP response headers, flush the enqueued body content
 * and release the resources used by the service. The connection could keep open depending
 * of the HTTP transaction.
 *
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: end_cb Defines a callback function to be invoked once the response object
 * finish flushing the pending data and clearing up the resources used.
 * @METHOD_RETURN: Upon successful completion it returns 0, otherwise it can generate an explicit
 * program exit due to bad API usage.
 */
int duda_response_end(duda_request_t *dr, void (*end_cb) (duda_request_t *))
{
    int ret;

    /* Make sure the caller set a valid HTTP response code */
    if (dr->sr->headers.status == 0 && dr->_st_http_headers_off == MK_FALSE) {
        duda_api_exception(dr, "Callback did not set the HTTP response status");
        abort();
    }

    dr->end_callback = end_cb;
    ret = duda_response_send_headers(dr);
    if (ret == -1) {
        return -1;
    }

    /* flush some enqueued content */
    ret = duda_queue_flush(dr);

    /*
     * The lesson of the day Feb 2, 2013: I must NEVER forget that when sending the
     * HTTP headers, Monkey sets the TCP_CORK flag ON in the socket in case the caller
     * wanted to send more data so we let know the Kernel to buffer a little more bytes.
     * If we do not set TCP_CORK to OFF we will face some delays in the response.
     *
     * KeepAlive was very slow due to this bug. More than 4 hours to found this silly bug.
     */
    mk_api->socket_cork_flag(dr->cs->socket, TCP_CORK_OFF);

    if (ret == 0) {
        duda_service_end(dr);
    }

    return 0;
}

/*
 * @METHOD_NAME: finalize
 * @METHOD_DESC: This is a virtual function which wraps the end() method. The only difference
 * is that invoking finalize() will not perform an explicit return. This function could be useful
 * inside a package context.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: end_cb Defines a callback function to be invoked once the response object
 * finish flushing the pending data and clearing up the resources used.
 * @METHOD_RETURN: Upon successful completion it returns 0, otherwise it can generate an explicit
 * program exit due to bad API usage.
 */
int duda_response_finalize(duda_request_t *dr, void (*end_cb) (duda_request_t *))
{
    (void) dr;
    (void) end_cb;

    return 0;
}

/*
 * @METHOD_NAME: flush
 * @METHOD_DESC: It flush the enqueued body content, this cover any data enqueued through
 * print(), printf() or sendfile() methods. This method should not be used inside the middle
 * of a callback routine, it's expected to be used at the end of a callback as the content
 * is flushed in asynchronous mode.
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_response_flush(duda_request_t *dr)
{
    return duda_queue_flush(dr);
}

struct duda_api_response *duda_response_object()
{
    struct duda_api_response *obj;

    obj = mk_api->mem_alloc(sizeof(struct duda_api_response));
    obj->send_headers        = duda_response_send_headers;
    obj->headers_off         = duda_response_headers_off;
    obj->http_status         = duda_response_http_status;
    obj->http_header         = duda_response_http_header;
    obj->http_header_n       = duda_response_http_header_n;
    obj->http_content_length = duda_response_http_content_length;
    obj->http_content_type   = duda_response_http_content_type;
    obj->print               = duda_response_print;
    obj->printf              = duda_response_printf;
    obj->sendfile            = duda_response_sendfile;
    obj->sendfile_range      = duda_response_sendfile_range;
    obj->wait                = duda_response_wait;
    obj->cont                = duda_response_continue;
    obj->_end                = duda_response_end;
    obj->_finalize           = duda_response_finalize;
    obj->flush               = duda_response_flush;

    return obj;
}
