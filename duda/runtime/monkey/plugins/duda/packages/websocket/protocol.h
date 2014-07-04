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

#ifndef DUDA_WEBSOCKET_PROT_H
#define DUDA_WEBSOCKET_PROT_H

/* GUID is defined by websockets v10 */
#define WS_GUID                    "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
#define WS_CONN_UPGRADE            "Upgrade"
#define WS_UPGRADE_WS              "websocket"

/* Request headers */
#define WS_HEADER_UPGRADE          "Upgrade:"
#define WS_HEADER_SEC_WS_ORIGIN    "Sec-WebSocket-Origin:"
#define WS_HEADER_SEC_WS_KEY       "Sec-WebSocket-Key:"
#define WS_HEADER_SEC_WS_VERSION   "Sec-WebSocket-Version:"
#define WS_HEADER_SEC_WS_PROTOCOL  "Sec-WebSocket-Protocol:"

/* Response headers */
#define WS_RESP_SWITCHING          "HTTP/1.1 101 Switching Protocols\r\n"
#define WS_RESP_UPGRADE            "Upgrade: websocket"
#define WS_RESP_CONNECTION         "Connection: Upgrade"
#define WS_RESP_WS_ACCEPT          "Sec-WebSocket-Accept: "

/*
 * Frame Opcode: data frames & control frames
 */

/* opcode: data frames    */
#define WS_OPCODE_CONTINUE   0x00
#define WS_OPCODE_TEXT       0x01
#define WS_OPCODE_BINARY     0x02

/* opcode: control frames */
#define WS_OPCODE_CLOSE      0x08
#define WS_OPCODE_PING       0x09
#define WS_OPCODE_PONG       0x0a

/* Framing macros */
#define WS_FRAME_MASK_LEN          4

#define CHECK_BIT(var, pos)        !!((var) & (1 << (pos)))

#define ANSI_BWHITE                ANSI_BOLD ANSI_WHITE
#define ANSI_RGREEN                ANSI_RESET ANSI_GREEN
#define FLAG_ON                    ANSI_BOLD ANSI_YELLOW " ON" ANSI_RGREEN
#define FLAG_OFF                   ANSI_BOLD ANSI_RED "OFF" ANSI_RGREEN

/* SHA1 stuff */
#define SHA1_DIGEST_LEN            20

#define ANSI_BWHITE                ANSI_BOLD ANSI_WHITE
#define ANSI_RGREEN                ANSI_RESET ANSI_GREEN

/* Websocket payload data type */
typedef unsigned char ws_msg_t;

#endif
