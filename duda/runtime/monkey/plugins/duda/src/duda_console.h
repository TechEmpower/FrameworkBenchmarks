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

#ifndef DUDA_CONSOLE_H
#define DUDA_CONSOLE_H

#include "duda.h"
#include "duda_api.h"

#define DD_HTML_HEADER                            \
    "<html>\n"                                    \
    "<head>\n"                                    \
    "    <title>Duda I/O - %s</title>\n"          \
    "%s\n"                                        \
    "</head>\n"

#define DD_HTML_CSS                                                     \
    "    <link href=\"/ddr/bootstrap/css/bootstrap.css\" rel=\"stylesheet\">\n" \
    "    <link href=\"/ddr/bootstrap/css/duda.css\" rel=\"stylesheet\">\n"

#define DD_HTML_NAVBAR_BASIC                                            \
    "<div class=\"navbar navbar-inverse navbar-fixed-top\" role=\"navigation\">\n" \
    "   <div class=\"container\">\n"                                    \
    "        <div class=\"navbar-header\">\n"                           \
    "            <button type=\"button\" class=\"navbar-toggle\" data-toggle=\"collapse\" data-target=\".navbar-collapse\">\n" \
    "                <span class=\"sr-only\">Toggle navigation</span>\n" \
    "                <span class=\"icon-bar\"></span>\n"                \
    "                <span class=\"icon-bar\"></span>\n"                \
    "                <span class=\"icon-bar\"></span>\n"                \
    "            </button>\n"                                           \
    "            <a class=\"navbar-brand\" href=\"#\">Duda I/O</a>\n"   \
    "        </div>\n"                                                  \
    "        <ul class=\"nav navbar-nav navbar-right\">"                \
    "            <li><a href=\"../navbar-fixed-top/\">%s</a></li>"      \
    "        </ul>"                                                     \
    "    </div>\n"                                                      \
    "</div>\n"

#define DD_HTML_FOOTER "</body></html>\n"

#define console_debug(dr, fmt, ...) duda_console_write(dr, __FILE__, __LINE__, fmt, ##__VA_ARGS__)

struct duda_api_console {
    #define debug(dr, fmt, ...) _debug(dr, __FILE__, __LINE__, fmt, ##__VA_ARGS__)
    void (*_debug) (duda_request_t *, char *, int, char *, ...);
};

struct duda_api_console *duda_console_object();
void duda_console_cb_messages(duda_request_t *dr);
void duda_console_cb_map(duda_request_t *dr);
void duda_console_write(duda_request_t *dr,
                        char *file, int line,
                        char *format, ...);

#endif
