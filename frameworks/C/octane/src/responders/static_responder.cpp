#include <stdlib.h>
#include <uv.h>
#include "static_responder.hpp"

void create_response_static(write_batch* batch) {
    batch->buffers[batch->number_of_used_buffers].base = RESPONSE;
    batch->buffers[batch->number_of_used_buffers].len = sizeof(RESPONSE);
    batch->number_of_used_buffers++;
}

void stream_on_read_static(connection* conn, size_t requests, uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
    uv_write_t *write_req = create_write_with_batch(requests);
    write_batch* batch = (write_batch*)write_req->data;

    for (int i=0; i<requests; i++) {
        create_response_static(batch);
    }

    if (uv_is_writable(stream)) {
        // TODO: Use the return values from uv_write()
        int rc = uv_write(write_req, stream, batch->buffers, batch->number_of_used_buffers, after_write_static);
    } else {
        // TODO: Handle closing the stream.
    }
}

void after_write_static(uv_write_t* req, int status) {
    free(req);
}

