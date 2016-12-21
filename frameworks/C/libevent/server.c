#include <stdio.h>
#include <string.h>
#include <event2/event.h>
#include <event2/http.h>
#include <event2/buffer.h>
#include "ccan/json/json.h"

#define HELLO_WORLD "Hello, World!"

void plaintext_cb(struct evhttp_request *req, void *arg) {
    struct evbuffer *evbuf = evhttp_request_get_output_buffer(req);
    evbuffer_add(evbuf, HELLO_WORLD, strlen(HELLO_WORLD)); // Set content

    // Set response headers
    evhttp_add_header(evhttp_request_get_output_headers(req), "Server", "libevent");
    evhttp_add_header(evhttp_request_get_output_headers(req), "Content-Type", "text/plain");

    evhttp_send_reply(req, 200, "OK", evbuf); // Send response
}

void json_cb(struct evhttp_request *req, void *arg) {
    struct evbuffer *evbuf = evhttp_request_get_output_buffer(req);

    // Set response headers
    evhttp_add_header(evhttp_request_get_output_headers(req), "Server", "libevent");
    evhttp_add_header(evhttp_request_get_output_headers(req), "Content-Type", "application/json");

    // Create JSON Object
    JsonNode *obj = json_mkobject();
    json_append_member(obj, "message", json_mkstring(HELLO_WORLD));
    char *payload = json_encode(obj);
    evbuffer_add(evbuf, payload, strlen(payload));
    
    // Free JSON object and payload
    json_delete(obj);
    free(payload);

    evhttp_send_reply(req, 200, "OK", evbuf); // Send response
}

void main() {
    printf("Creating event base");
    struct event_base *evb = event_base_new();
    struct evhttp *eh = evhttp_new(evb);

    evhttp_set_cb(eh, "/plaintext", plaintext_cb, NULL);
    evhttp_set_cb(eh, "/json", json_cb, NULL);

    struct evhttp_bound_socket *handle = evhttp_bind_socket_with_handle(eh, "0.0.0.0", 8080);
    event_base_dispatch(evb);
    event_base_free(evb);
}
