/*
 Copyright (c) 2016 Anton Valentinov Kirilov

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef WORLD_H_

#define WORLD_H_

#include <h2o.h>

#include "global_data.h"
#include "list.h"
#include "request_handler_data.h"
#include "thread.h"

void cleanup_world_handler_thread_data(request_handler_thread_data_t *data);
void cleanup_world_handlers(request_handler_data_t *data);
void initialize_world_handler_thread_data(thread_context_t *ctx,
                                          const request_handler_data_t *data,
                                          request_handler_thread_data_t *thread_data);
void initialize_world_handlers(const config_t *config,
                               h2o_hostconf_t *hostconf,
                               h2o_access_log_filehandle_t *log_handle,
                               list_t **postinitialization_tasks,
                               request_handler_data_t *data);

#endif // WORLD_H_
