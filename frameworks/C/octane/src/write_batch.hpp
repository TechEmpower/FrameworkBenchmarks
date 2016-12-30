#pragma once
#include <stdlib.h>
#include <uv.h>

typedef struct {
    uv_buf_t* buffers;
    size_t number_of_used_buffers;
    size_t number_of_total_buffers;
}write_batch;

uv_write_t* create_write_with_batch(size_t number_of_total_buffers);
write_batch* get_write_batch(uv_write_t* write_req);