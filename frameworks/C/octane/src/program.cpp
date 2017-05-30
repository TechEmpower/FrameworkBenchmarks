#include <stdio.h>
#include <uv.h>
#include <stdbool.h>
#include <octane.h>
#include "responders/sds_responder.hpp"

void timer_callback(uv_timer_t* timer);

void on_request(http_connection* connection, http_request** requests, int number_of_requests);

char* current_time;
uv_timer_t timer;

int main(int argc, char *argv[]) {
    http_listener* listener = new_http_listener();
    uv_timer_init(listener->loop, &timer);
    uv_timer_start(&timer, timer_callback, 0, 500);

    begin_listening(listener, "0.0.0.0", 8000, false, 40, 128, NULL, NULL, NULL, on_request);

    printf("Listening...\n");
}

void on_request(http_connection* connection, http_request** requests, int number_of_requests) {
    write_batch* batch = create_write_batch(number_of_requests);

    for (int i=0; i<number_of_requests; i++) {
        if (requests[i]->path[1] == 'p') {
            create_plaintext_response_sds(batch);
        } else if (requests[i]->path[1] == 'j') {
            create_json_response_sds(batch);
        }
    }
    if (http_connection_is_writable(connection)) {
        // TODO: Use the return values from uv_write()
        int rc = http_connection_write(connection, batch);
    } else {
        // TODO: Handle closing the stream.
    }

    free_http_requests(requests, number_of_requests);
}

void timer_callback(uv_timer_t* timer) {
    time_t curtime;
    time(&curtime);
    char* time = ctime(&curtime);
    current_time = time;
}
