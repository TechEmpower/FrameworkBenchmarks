#pragma once

#include <uv.h>
#include "../connection.hpp"
#include "../write_batch.hpp"

#define RESPONSE \
    "HTTP/1.1 200 OK\r\n" \
    "Server: octane\r\n" \
    "Date: Mon Dec 12 00:00:00 2016\r\n" \
    "Content-Type: text/plain\r\n" \
    "Content-Length: 15\r\n" \
    "\r\n" \
    "Hello, World!\n"

void create_response_static(write_batch* batch);
void stream_on_read_static(connection* conn, size_t requests, uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf);
void after_write_static(uv_write_t* req, int status);