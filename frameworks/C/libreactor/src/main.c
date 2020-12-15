#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <err.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>

#include "setup.h"
#include "helpers.h"

#define JSON_PREAMBLE "HTTP/1.1 200 OK\r\n"\
                      "Server: libreactor\r\n"\
                      "Content-Type: application/json\r\n"

#define TEXT_PREAMBLE "HTTP/1.1 200 OK\r\n"\
                      "Server: libreactor\r\n"\
                      "Content-Type: text/plain\r\n"

void plaintext(server_context *context, char *response)
{
  static const segment text_preamble = { .base = TEXT_PREAMBLE, .size = sizeof(TEXT_PREAMBLE) - 1 };
  write_response(&context->session->stream, text_preamble, segment_string(response));
}

void json(server_context *context, clo *json_object)
{
  static const segment json_preamble = { .base = JSON_PREAMBLE, .size = sizeof(JSON_PREAMBLE) - 1 };
  static char json_string[4096];

  (void) clo_encode(json_object, json_string, sizeof(json_string));
  write_response(&context->session->stream, json_preamble, segment_string(json_string));
}

static core_status server_handler(core_event *event)
{
  static char hello_string[] = "Hello, World!";
  static char default_string[] = "Hello from libreactor!\n";
  static clo_pair json_pair[] = {{ .string = "message", .value = { .type = CLO_STRING, .string = "Hello, World!" }}};
  static clo json_object[] = {{ .type = CLO_OBJECT, .object = json_pair }};

  server *server = event->state;
  server_context *context = (server_context *) event->data;

  if (event->type == SERVER_REQUEST){
    if (segment_equal(context->request.target, segment_string("/json"))){
      json(context, json_object);
    }
    else if (segment_equal(context->request.target, segment_string("/plaintext"))){
      plaintext(context, hello_string);
    }
    else{
      plaintext(context, default_string);
    }
    return CORE_OK;
  }
  else {
    warn("error");
    server_destruct(server);
    return CORE_ABORT;
  }
}

int main()
{
  server s;

  setup();
  core_construct(NULL);
  server_construct(&s, server_handler, &s);
  server_open(&s, 0, 8080);

  core_loop(NULL);
  core_destruct(NULL);
}