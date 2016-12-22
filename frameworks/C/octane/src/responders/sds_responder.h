#pragma once

#include <uv.h>
#include "../connection.h"
#include "../write_batch.h"

void create_response_sds(write_batch* batch);
void stream_on_read_sds(connection* conn, size_t requests, uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf);
void after_write_sds(uv_write_t* req, int status);