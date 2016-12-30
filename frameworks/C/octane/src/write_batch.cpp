#include <stdlib.h>
#include "write_batch.hpp"
#include "common.hpp"

uv_write_t* create_write_with_batch(size_t number_of_total_buffers) {
    /*
     * Allocate the 3 structures and array of buffers in one malloc call
     * to reduce malloc() contention across CPU cores.
     */
    uv_write_t *write_req;
    size_t bytes = sizeof(*write_req) + sizeof(write_batch) + (sizeof(uv_buf_t) * number_of_total_buffers);
    if(!(write_req = (uv_write_t *) malloc(bytes))){
        memory_error("Unable to allocate buffer of size %d", bytes);
    }

    write_batch* batch = get_write_batch(write_req);
    batch->buffers = (uv_buf_t*)(batch + 1);
    batch->number_of_used_buffers = 0;
    batch->number_of_total_buffers = number_of_total_buffers;
    write_req->data = batch;

    return write_req;
}

write_batch* get_write_batch(uv_write_t* write_req) {
    write_batch* batch = (write_batch*)(write_req + 1);
    return batch;
}