#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <err.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>

#include "setup.h"

static core_status server_handler(core_event *event)
{
  static char json_msg[4096];

  server *server = event->state;
  server_context *context = (server_context *) event->data;

  if (event->type == SERVER_REQUEST){
    if (segment_equal(context->request.target, segment_string("/json"))){
      (void) clo_encode((clo[]) {clo_object({"message", clo_string("Hello, World!")})}, json_msg, sizeof(json_msg));
      server_ok(context, segment_string("application/json"), segment_string(json_msg));
    }
    else if (segment_equal(context->request.target, segment_string("/plaintext"))){
      server_ok(context, segment_string("text/plain"), segment_string("Hello, World!"));
    }
    else{
      server_ok(context, segment_string("text/plain"), segment_string("Hello from libreactor!\n"));
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