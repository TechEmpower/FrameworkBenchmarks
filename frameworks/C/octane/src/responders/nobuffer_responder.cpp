#include <stdlib.h>
#include <uv.h>
#include "nobuffer_responder.hpp"
#include "../write_batch.hpp"

void create_response_nobuffer(write_batch* batch) {
    batch->buffers[batch->number_of_used_buffers].base = "HTTP/1.1 200 OK\r\n";
    batch->buffers[batch->number_of_used_buffers].len = 17;
    batch->number_of_used_buffers++;

    batch->buffers[batch->number_of_used_buffers].base = "Server: octane\r\n";
    batch->buffers[batch->number_of_used_buffers].len = 28;
    batch->number_of_used_buffers++;

    batch->buffers[batch->number_of_used_buffers].base = "Content-Type: text/plain\r\n";
    batch->buffers[batch->number_of_used_buffers].len = 26;
    batch->number_of_used_buffers++;

    batch->buffers[batch->number_of_used_buffers].base = "Content-Length: 15\r\n";
    batch->buffers[batch->number_of_used_buffers].len = 20;
    batch->number_of_used_buffers++;

    batch->buffers[batch->number_of_used_buffers].base = "Date: Mon Dec 12 00:00:00 2016\r\n";
    batch->buffers[batch->number_of_used_buffers].len = 32;
    batch->number_of_used_buffers++;

    batch->buffers[batch->number_of_used_buffers].base = "\r\n";
    batch->buffers[batch->number_of_used_buffers].len = 2;
    batch->number_of_used_buffers++;

    batch->buffers[batch->number_of_used_buffers].base = "Hello, World!\n\n";
    batch->buffers[batch->number_of_used_buffers].len = 15;
    batch->number_of_used_buffers++;
}

void stream_on_read_nobuffer(connection* conn, size_t requests, uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
    uv_write_t *write_req = create_write_with_batch(requests * 7);
    write_batch* batch = (write_batch*)write_req->data;

    for (int i=0; i<requests; i++) {
        create_response_nobuffer(batch);
    }

    if (uv_is_writable(stream)) {
        // TODO: Use the return values from uv_write()
        int rc = uv_write(write_req, stream, batch->buffers, batch->number_of_used_buffers, after_write_nobuffer);
    } else {
        // TODO: Handle closing the stream.
    }
}

void after_write_nobuffer(uv_write_t* req, int status) {
    free(req);
}
