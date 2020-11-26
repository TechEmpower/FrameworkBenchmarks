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

struct http_server
{
  reactor_net    net;
  list           connections;
};

static void plaintext(reactor_stream *stream, char *response)
{
  static const reactor_vector text_preamble = { .base = TEXT_PREAMBLE, .size = sizeof(TEXT_PREAMBLE) - 1 };
  write_response(stream, text_preamble, reactor_vector_string(response));
}

static void json(reactor_stream *stream, clo *json_object)
{
  static const reactor_vector json_preamble = { .base = JSON_PREAMBLE, .size = sizeof(JSON_PREAMBLE) - 1 };
  static char json_string[4096];

  (void) clo_encode(json_object, json_string, sizeof(json_string));
  write_response(stream, json_preamble, reactor_vector_string(json_string));
}

static reactor_status http_handler(reactor_event *event)
{
  static char hello_string[] = "Hello, World!";
  static char default_string[] = "Hello from libreactor!\n";
  static clo_pair json_pair[] = {{ .string = "message", .value = { .type = CLO_STRING, .string = "Hello, World!" }}};
  static clo json_object[] = {{ .type = CLO_OBJECT, .object = json_pair }};

  reactor_http *http_connection = event->state;
  reactor_http_request *request;

  if (event->type == REACTOR_HTTP_EVENT_REQUEST){
    request = (reactor_http_request *) event->data;

    if (reactor_vector_equal(request->target, reactor_vector_string("/json"))){
      json(&http_connection->stream, json_object);
    }
    else if (reactor_vector_equal(request->target, reactor_vector_string("/plaintext"))){
      plaintext(&http_connection->stream, hello_string);
    }
    else{
      plaintext(&http_connection->stream, default_string);
    }
    return REACTOR_OK;
  }
  else {
    reactor_http_destruct(http_connection);
    list_erase(http_connection, NULL);
    return REACTOR_ABORT;
  }
}

static reactor_status net_handler(reactor_event *event)
{
  struct http_server *server = event->state;
  reactor_http connection_initializer = (reactor_http) {0};
  reactor_http *http_connection;

  switch (event->type)
    {
    case REACTOR_NET_EVENT_ACCEPT:
      http_connection = list_push_back(&server->connections, &connection_initializer, sizeof (reactor_http));
      reactor_http_construct(http_connection, http_handler, http_connection);
      reactor_http_set_mode(http_connection, REACTOR_HTTP_MODE_REQUEST);
      reactor_http_open(http_connection, event->data);
      return REACTOR_OK;
    default:
      reactor_net_destruct(&server->net);
      return REACTOR_ABORT;
    }
}

static reactor_status http_date_timer_handler(reactor_event *event)
{
  (void) event;
  http_date_header(1); // update the date header
  return REACTOR_OK;
}

int main()
{
  struct http_server server = {0};
  reactor_timer timer;

  setup();
  list_construct(&server.connections);
  reactor_core_construct();

  // Set the correct date before the server starts. Timer then updates it every second
  http_date_header(1);
  reactor_timer_construct(&timer, http_date_timer_handler, &timer);
  reactor_timer_set(&timer, 1, 1000000000);

  reactor_net_construct(&server.net, net_handler, &server);
  (void) custom_reactor_net_bind(&server.net, "0.0.0.0", "8080");

  reactor_core_run();
  reactor_core_destruct();
}