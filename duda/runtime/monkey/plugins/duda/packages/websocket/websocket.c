/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>.
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

/*
 * @OBJ_NAME: websocket
 * @OBJ_MENU: Web Sockets
 * @OBJ_DESC: This package implements the Websocket protocol as described in
 * the RFC 6455. It allow to define callbacks on specifics events and also implements
 * a broadcaster service to make easier distribute message among different active
 * connections.
 * @PKG_HEADER: #include "packages/websocket/websocket.h"
 * @PKG_INIT: duda_load_package(websocket, "websocket");
 *     ...
 *     ||*
 *      * In case you want to use the websocket broadcast feature you must enable the
 *      * broadcaster service:
 *      *||
 *     websocket->broadcaster();
 *
 */

#define _GNU_SOURCE

/* Common  */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

/* Networking - I/O*/
#include <fcntl.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

/* Plugin */
#include "duda_objects.h"
#include "duda_event.h"
#include "request.h"
#include "duda_api.h"
#include "sha1.h"
#include "base64.h"
#include "websocket.h"
#include "protocol.h"
#include "callbacks.h"

#define ws_invalid_upgrade(dr) response->http_status(dr, 400);  \
    response->finalize(dr, NULL);                               \
    return -1;

#ifdef PLUGIN_TRACE
#undef PLUGIN_TRACE
#define PLUGIN_TRACE   printf
#endif

/*
 * Internal websocket package callback functions
 *
 * FIXME: This function is only handling messages of one frame size. It should be
 * able to get multiple frames and assemble them
 */

int cb_ws_read(int sockfd, void *data)
{
    uint64_t i;
    int n;
    int paysize = 256;
    unsigned char buf[256];
    unsigned int frame_size = 0;
    unsigned int frame_opcode = 0;
    unsigned int frame_mask = 0;
    unsigned char *frame_payload;
    unsigned char frame_masking_key[WS_FRAME_MASK_LEN];
    uint64_t payload_length = 0;
    unsigned int masking_key_offset = 0;
    ws_request_t *wr;
    duda_request_t *dr = data;

    wr = ws_request_get(sockfd);
    if (!wr){
        PLUGIN_TRACE("[FD %i] this FD is not a WebSocket Frame", sockfd);
        return DUDA_EVENT_CLOSE;
    }

    memset(buf, '\0', sizeof(buf));
    n = monkey->socket_read(dr->socket, buf, 256);
    if (n <= 0) {
        return DUDA_EVENT_CLOSE;
    }

    frame_size     = n;
    frame_opcode   = buf[0] & 0x0f;
    frame_mask     = CHECK_BIT(buf[1], 7);
    payload_length = buf[1] & 0x7f;

    if (payload_length == 126) {
        payload_length = buf[2] * 256 + buf[3];
        masking_key_offset = 4;
    }
    else if (payload_length == 127) {
        memcpy(&payload_length, buf + 2, 8);
        masking_key_offset = 10;
    }
    else {
        masking_key_offset = 2;
    }

#ifdef TRACE
    PLUGIN_TRACE("%s> Frame data\n%s%s", ANSI_BOLD, ANSI_RESET, ANSI_GREEN);
    printf("  FIN  %37s\n", CHECK_BIT(buf[0], 7) ? FLAG_ON : FLAG_OFF);
    printf("  RSV1 %37s\n", CHECK_BIT(buf[0], 6) ? FLAG_ON : FLAG_OFF);
    printf("  RSV2 %37s\n", CHECK_BIT(buf[0], 5) ? FLAG_ON : FLAG_OFF);
    printf("  RSV3 %37s\n", CHECK_BIT(buf[0], 4) ? FLAG_ON : FLAG_OFF);
    printf("  Op Code         %s%8i%s\n", ANSI_BWHITE, frame_opcode, ANSI_RGREEN);
    printf("  Masked          %s%8i%s\n", ANSI_BWHITE, frame_mask, ANSI_RGREEN);
    printf("  Frame Size      %s%8i%s\n", ANSI_BWHITE, frame_size, ANSI_RGREEN);
    printf("  Payload Length  %s%8i%s\n", ANSI_BWHITE, (unsigned int) payload_length, ANSI_RGREEN);
    printf("  Mask Key Offset %s%8i%s\n", ANSI_BWHITE, (unsigned int) masking_key_offset, ANSI_RGREEN);
    printf("%s%s> --- --- --- --- <%s\n", ANSI_RESET,  ANSI_BOLD, ANSI_RESET);
    fflush(stdout);
#endif

    wr->opcode      = frame_opcode;
    wr->payload_len = payload_length;
    wr->payload     = monkey->mem_alloc_z(paysize);

    if (frame_mask) {
        memcpy(frame_masking_key, buf + masking_key_offset, WS_FRAME_MASK_LEN);

        if (payload_length != (frame_size - (masking_key_offset + WS_FRAME_MASK_LEN))) {
            /* FIXME: add some message for the invalid frame size */
        }

        /* Unmasking the frame payload */
        frame_payload = buf + masking_key_offset + WS_FRAME_MASK_LEN;
        for (i = 0; i < payload_length; i++) {
            wr->payload[i] = frame_payload[i] ^ frame_masking_key[i & 0x03];
        }
    }
    else {
        /* There is no masking key (odd because it must be), get the frame payload */
        frame_payload = buf + masking_key_offset;
        memcpy(wr->payload, frame_payload, payload_length);
    }

    if (wr->opcode == WS_OPCODE_CONTINUE || wr->opcode == WS_OPCODE_TEXT ||
        wr->opcode == WS_OPCODE_BINARY) {
        if (wr->cb_on_message) {
            wr->cb_on_message(dr, wr);
        }
    }
    else if (wr->opcode == WS_OPCODE_CLOSE) {
        if (wr->cb_on_close) {
            wr->cb_on_close(dr, wr);
        }
        /*
         * Per protocol spec:
         *
         * 5.5.1.  Close
         * ...
         * If an endpoint receives a Close frame and did not previously send a
         * Close frame, the endpoint MUST send a Close frame in response.  (When
         * sending a Close frame in response, the endpoint typically echos the
         * status code it received.)  It SHOULD do so as soon as practical.  An
         * endpoint MAY delay sending a Close frame until its current message is
         * sent (for instance, if the majority of a fragmented message is
         * already sent, an endpoint MAY send the remaining fragments before
         * sending a Close frame).  However, there is no guarantee that the
         * endpoint that has already sent a Close frame will continue to process
         * data.
         */
        ws_send_data(sockfd, 1, 0, 0, 0, WS_OPCODE_CLOSE, 0, NULL);
        return DUDA_EVENT_CLOSE;
    }

    return DUDA_EVENT_OWNED;
}

int cb_ws_error(int sockfd, void *data)
{
    ws_request_t *wr;
    duda_request_t *dr = data;

    wr = ws_request_get(sockfd);
    if (!wr){
        PLUGIN_TRACE("[FD %i] this FD is not a WebSocket Frame", sockfd);
        return DUDA_EVENT_CLOSE;
    }

    if (wr->cb_on_error) {
        wr->cb_on_error(dr, wr);
    }

    return DUDA_EVENT_OWNED;
}

int cb_ws_close(int sockfd, void *data)
{
    ws_request_t *wr;
    duda_request_t *dr = data;

    wr = ws_request_get(sockfd);
    if (!wr){
        PLUGIN_TRACE("[FD %i] this FD is not a WebSocket Frame", sockfd);
        return DUDA_EVENT_CLOSE;
    }

    if (wr->cb_on_close) {
        wr->cb_on_close(dr, wr);
    }
    return DUDA_EVENT_CLOSE;
}

int cb_ws_timeout(int sockfd, void *data)
{
    ws_request_t *wr;
    duda_request_t *dr = data;

    wr = ws_request_get(sockfd);
    if (!wr){
        PLUGIN_TRACE("[FD %i] this FD is not a WebSocket Frame", sockfd);
        return DUDA_EVENT_CLOSE;
    }

    if (wr->cb_on_timeout) {
        wr->cb_on_timeout(dr, wr);
    }
    return DUDA_EVENT_OWNED;
}


/*
 * @METHOD_NAME: handshake
 * @METHOD_DESC: It perform the websocket handshake and connection upgrade. This must be
 * used inside a normal HTTP callback function. It will take care of the handshake details
 * and response data.
 * @METHOD_PROTO: int handshake(duda_request_t *dr, int channel)
 * @METHOD_PARAM: dr the request context information hold by a duda_request_t type
 * @METHOD_PARAM: channel specify the channel number where the connection will be associated. Use -1 to specify all channels.
 * @METHOD_RETURN:  Upon successful completion it returns 0, on error returns -1.
 */
int ws_handshake(duda_request_t *dr, int channel)
{
    int len;
    size_t out_len;
    char buffer[256];
    char accept_token[256];
    char *row = NULL;

    int key_len;
    char *ws_key;

    struct ws_request *wr_node;
    unsigned char digest[SHA1_DIGEST_LEN];
    unsigned char *encoded_accept = NULL;
    SHA_CTX sha; /* defined in sha1/sha1.h */

    wr_node = ws_request_get(dr->socket);
    if (wr_node) {
        ws_request_delete(dr->socket);
        wr_node = NULL;
    }

    /*
     * We only accept a new request who have specified a connection
     * upgrade to talk in Websocket protocol
     */
    if (!wr_node && dr->sr->connection.data) {
        if (monkey->str_search_n(dr->sr->connection.data,
                                 WS_CONN_UPGRADE,
                                 MK_STR_INSENSITIVE, dr->sr->connection.len) < 0) {
            ws_invalid_upgrade(dr);
        }

        PLUGIN_TRACE("[FD %i] WebSockets Connection Upgrade\n", dr->socket);

        /* Get upgrade type */
        row = request->header_get(dr, WS_HEADER_UPGRADE);
        if (strncasecmp(row, WS_UPGRADE_WS, sizeof(WS_UPGRADE_WS) - 1) != 0) {
            ws_invalid_upgrade(dr);
        }

        PLUGIN_TRACE("[FD %i] WebSockets Upgrade to 'websocket'\n", dr->socket);

        /* Validate Sec-WebSocket-Key */
        ws_key = request->header_get(dr, WS_HEADER_SEC_WS_KEY);

        if (!ws_key) {
            PLUGIN_TRACE("[FD %i] WebSockets missing key\n", dr->socket);
            ws_invalid_upgrade(dr);
        }

        monkey->event_socket_change_mode(dr->socket, MK_EPOLL_RW, MK_EPOLL_LEVEL_TRIGGERED);

        /* Ok Baby, Handshake time! */
        key_len = strlen(ws_key);
        strncpy(buffer, ws_key, key_len);

        /* Websockets GUID */
        strncpy(buffer + key_len, WS_GUID, sizeof(WS_GUID) - 1);
        buffer[key_len + sizeof(WS_GUID) - 1] = '\0';

        /* Buffer to sha1() */
        SHA1_Init(&sha);
        SHA1_Update(&sha, buffer, strlen(buffer));
        SHA1_Final(digest, &sha);

        /* Encode accept key with base64 */
        encoded_accept = base64_encode(digest, SHA1_DIGEST_LEN, &out_len);
        encoded_accept[out_len] = '\0';

        /* Set a custom response status */
        strncpy(buffer, WS_RESP_SWITCHING, sizeof(WS_RESP_SWITCHING) - 1);

        dr->sr->headers.status = MK_CUSTOM_STATUS;
        dr->sr->headers.custom_status.data = buffer;
        dr->sr->headers.custom_status.len  = (sizeof(WS_RESP_SWITCHING) -1);

        /* Monkey core must not handle the Connection header */
        dr->sr->headers.connection = -1;

        /* Set 'Upgrade: websocket' */
        monkey->header_add(dr->sr, WS_RESP_UPGRADE, sizeof(WS_RESP_UPGRADE) - 1);

        /* Set 'Connection: upgrade' */
        monkey->header_add(dr->sr, WS_RESP_CONNECTION, sizeof(WS_RESP_CONNECTION) - 1);

        /* Compose accept token */
        len = sizeof(WS_RESP_WS_ACCEPT) - 1;
        strncpy(accept_token, WS_RESP_WS_ACCEPT, len);
        strncpy(accept_token + len, (char *) encoded_accept, out_len);
        len += out_len - 1;
        accept_token[len] = '\0';

        /* Add accept token to response headers */
        monkey->header_add(dr->sr, accept_token, len);

        /* Send response HTTP headers */
        response->send_headers(dr);

        /* Free block used by base64_encode() */
        monkey->mem_free(encoded_accept);

        /* Register node in main list */
        wr_node = ws_request_create(dr->socket,
                                    channel,
                                    dr,
                                    ws_callbacks->on_open,
                                    ws_callbacks->on_message,
                                    ws_callbacks->on_error,
                                    ws_callbacks->on_close,
                                    ws_callbacks->on_timeout);
        ws_request_add(wr_node);

        /* Register socket with plugin events interface */
        event->add(dr->socket, DUDA_EVENT_READ, DUDA_EVENT_LEVEL_TRIGGERED,
                   cb_ws_read, NULL, cb_ws_error, cb_ws_close, cb_ws_timeout, dr);

        /* provide request handle by calling on_open */
        if (wr_node->cb_on_open) {
            wr_node->cb_on_open(dr, wr_node);
        }

        return 0;
    }

    /* Send exception to the client */
    response->http_status(dr, 400);
    response->finalize(dr, NULL);

    return -1;
}

int ws_send_data(int sockfd,
                unsigned int fin,
                unsigned int rsv1,
                unsigned int rsv2,
                unsigned int rsv3,
                unsigned int opcode,
                uint64_t payload_len,
                unsigned char *payload_data)
{
    unsigned char buf[256];
    unsigned int offset = 0;


    /*
     * Per protocol spec on RFC6455, when the server sends a websocket
     * frame to the client it must NOT be masked:
     *
     * 5.  Data Framing
     *
     * 5.1.  Overview
     * .......
     * ....... A server MUST NOT mask any frames that it sends to
     * the client.  A client MUST close a connection if it detects a masked
     * frame....
     */
    unsigned int frame_mask = 0;
    int n;

    memset(buf, 0, sizeof(buf));
    buf[0] |= ((fin << 7) | (rsv1 << 6) | (rsv2 << 5) | (rsv3 << 4) | opcode);

    if (payload_len < 126) {
        buf[1] |= ((frame_mask << 7) | payload_len);
        offset = 2;
    }
    else if (payload_len >= 126 && payload_len <= 0xFFFF) {
        buf[1] |= ((frame_mask << 7) | 126);
        buf[2] = payload_len >> 8;
        buf[3] = payload_len & 0x0F;
        offset = 4;
    }
    else {
        buf[1] |= ((frame_mask << 7) | 127);
        memcpy(buf + 2, &payload_len, 8);
        offset = 10;
    }

    memcpy(buf + offset, payload_data, payload_len);

    n = monkey->socket_send(sockfd, buf, offset + payload_len);
    if (n <= 0) {
        return -1;
    }

    return n;
}

/*
 * @METHOD_NAME: write
 * @METHOD_DESC: It writes a message frame to a specified websocket connection.
 * @METHOD_PROTO: int write(struct ws_request *wr, unsigned int code, unsigned char *data, uint64_t len)
 * @METHOD_PARAM: wr the target websocket connection
 * @METHOD_PARAM: code the message code, can be one of: WS_OPCODE_CONTINUE, WS_OPCODE_TEXT, WS_OPCODE_BINARY, WS_OPCODE_CLOSE, WS_OPCODE_PING or WS_OPCODE_PONG.
 * @METHOD_PARAM: data the data to be send
 * @METHOD_PARAM: len the length of the data to be send
 * @METHOD_RETURN:  Upon successful completion it returns 0, on error returns -1.
 */
int ws_write(struct ws_request *wr, unsigned int code, unsigned char *data, uint64_t len)
{
    return ws_send_data(wr->socket, 1, 0, 0, 0, code, len, data);
}
