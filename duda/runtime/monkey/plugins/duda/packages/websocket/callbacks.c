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

#include "duda_api.h"
#include "callbacks.h"

/*
 * @METHOD_NAME: set_callback
 * @METHOD_DESC: It set a specific callback function against on a received event.
 * @METHOD_PROTO: int set_callback(int type, void (*callback) (duda_request_t *, struct ws_request *))
 * @METHOD_PARAM: type it specifies one of the following event types: WS_ON_MESSAGE, WS_ON_ERROR, WS_ON_CLOSE or WS_ON_TIMEOUT.
 * @METHOD_PARAM: callback the callback function
 * @METHOD_RETURN: It returns zero when the callback is set, otherwise -1 on errir
 */

int ws_set_callback(int type, void (*callback) (duda_request_t *, struct ws_request *))
{
    switch (type) {
        case WS_ON_OPEN:
            ws_callbacks->on_open = callback;
            break;
        case WS_ON_MESSAGE:
            ws_callbacks->on_message = callback;
            break;
        case WS_ON_ERROR:
            ws_callbacks->on_error = callback;
            break;
        case WS_ON_CLOSE:
            ws_callbacks->on_close = callback;
            break;
        case WS_ON_TIMEOUT:
            ws_callbacks->on_timeout = callback;
            break;
        default:
            printf("Error: invalid callback type %i\n", type);
            return -1;
        };

    return 0;
}
