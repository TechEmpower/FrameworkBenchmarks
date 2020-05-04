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

enum response_type
{
 JSON = 0,
 PLAINTEXT = 1
};
typedef enum response_type response_type;

struct http_server
{
  reactor_net    net;
  list           connections;
};

static char json_preamble[] = "HTTP/1.1 200 OK\r\n"
                              "Server: libreactor\r\n"
                              "Content-Type: application/json\r\n"
                              "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n"
                              "Content-Length: ";

static char plaintext_preamble[] = "HTTP/1.1 200 OK\r\n"
                                   "Server: libreactor\r\n"
                                   "Content-Type: text/plain\r\n"
                                   "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n"
                                   "Content-Length: ";

static size_t json_preamble_size = sizeof(json_preamble) - 1;
static size_t plaintext_preamble_size = sizeof(plaintext_preamble) - 1;


static int send_response(reactor_http *http_connection, char *response, response_type type)
{
  reactor_vector body = reactor_vector_string(response);
  reactor_vector content_length = reactor_utility_u32tov(body.size);

  if (type == JSON){
    reactor_stream_write(&http_connection->stream, json_preamble, json_preamble_size);
  }
  else {
    reactor_stream_write(&http_connection->stream, plaintext_preamble, plaintext_preamble_size);
  }

  reactor_stream_write(&http_connection->stream, content_length.base, content_length.size);
  reactor_stream_write(&http_connection->stream, "\r\n\r\n", 4);
  reactor_stream_write(&http_connection->stream, body.base, body.size);
  return REACTOR_OK;
}

static reactor_status http_handler(reactor_event *event)
{
  reactor_http *http_connection = event->state;
  reactor_http_request *request;
  char json_msg[4096];

  if (event->type == REACTOR_HTTP_EVENT_REQUEST){
    request = (reactor_http_request *) event->data;

    if (reactor_vector_equal(request->target, reactor_vector_string("/json"))){
      (void) clo_encode((clo[]) {clo_object({"message", clo_string("Hello, World!")})}, json_msg, sizeof(json_msg));
      return send_response(http_connection, json_msg, JSON);
    }
    else if (reactor_vector_equal(request->target, reactor_vector_string("/plaintext"))){
      return send_response(http_connection, "Hello, World!", PLAINTEXT);
    }
    else{
      return send_response(http_connection, "Hello from libreactor!\n", PLAINTEXT);
    }
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

static void set_date()
{
  update_date();
  copy_date(json_preamble, 69);
  copy_date(plaintext_preamble, 63);
}

static reactor_status http_date_timer_handler(reactor_event *event)
{
  (void) event;
  set_date();
  return REACTOR_OK;
}


int main()
{
  struct http_server server = {0};
  reactor_timer timer;

  setup();
  list_construct(&server.connections);
  reactor_core_construct();

  // set the correct date in the preambles before the server starts. Timer then updates them every second
  set_date();
  reactor_timer_construct(&timer, http_date_timer_handler, &timer);
  reactor_timer_set(&timer, 1, 1000000000);

  reactor_net_construct(&server.net, net_handler, &server);
  (void) custom_reactor_net_bind(&server.net, "0.0.0.0", "8080");

  reactor_core_run();
  reactor_core_destruct();
}