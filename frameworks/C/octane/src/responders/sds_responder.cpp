#include <string.h>
#include <uv.h>
#include "sds.h"
#include "sds_responder.hpp"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace rapidjson;

extern char* current_time;

const char* PLAINTEXT_CONTENT = "Hello, World!";

void create_plaintext_response_sds(write_batch* batch) {
    sds response_buffer = sdsnew("HTTP/1.1 200 OK\r\n");
    response_buffer = sdscat(response_buffer, "Server: octane\r\n");
    response_buffer = sdscat(response_buffer, "Content-Type: text/plain\r\n");
    response_buffer = sdscatprintf(response_buffer, "Content-Length: %d\r\n", strlen(PLAINTEXT_CONTENT));
    response_buffer = sdscatprintf(response_buffer, "Date: %s\r\n", current_time);
    response_buffer = sdscat(response_buffer, PLAINTEXT_CONTENT);

    batch->buffers[batch->number_of_used_buffers].base = response_buffer;
    batch->buffers[batch->number_of_used_buffers].len = sdslen(response_buffer);
    batch->number_of_used_buffers++;
}

void create_json_response_sds(write_batch* batch) {
    // Taken from H20 implementation.
    // volatile is used to ensure that the object is instantiated every time
    // the function is called.
    const volatile struct {
        const char *message;
    } object = {PLAINTEXT_CONTENT};

    StringBuffer s;
    Writer<StringBuffer> writer(s);
    writer.StartObject();
    writer.Key("message");
    writer.String((const char *)object.message, strlen(object.message));
    writer.EndObject();

    sds response_buffer = sdsnew("HTTP/1.1 200 OK\r\n");
    response_buffer = sdscat(response_buffer, "Server: octane\r\n");
    response_buffer = sdscat(response_buffer, "Content-Type: application/json\r\n");
    response_buffer = sdscatprintf(response_buffer, "Content-Length: %d\r\n", s.GetSize());
    response_buffer = sdscatprintf(response_buffer, "Date: %s\r\n", current_time);
    response_buffer = sdscat(response_buffer, s.GetString());

    batch->buffers[batch->number_of_used_buffers].base = response_buffer;
    batch->buffers[batch->number_of_used_buffers].len = sdslen(response_buffer);
    batch->number_of_used_buffers++;
}
