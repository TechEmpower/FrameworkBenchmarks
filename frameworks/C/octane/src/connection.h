#pragma once

#include <stdbool.h>

typedef struct
{
    uv_tcp_t stream;
    enum {OPEN, CLOSING, CLOSED} state;
    void* data;
    bool keep_alive;
    int bytes_remaining;
    int request_length;
} connection;

typedef enum {
    OK,
    SIZE_EXCEEDED,
    BAD_REQUEST,
    INTERNAL_ERROR
} request_state;

connection* create_connection();
void free_connection(connection* conn);

char* current_time;
