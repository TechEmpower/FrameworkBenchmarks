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

static reactor_status tfb(reactor_event *event)
{
  reactor_server_session *session = (reactor_server_session *) event->data;

  if (reactor_vector_equal(session->request->target, reactor_vector_string("/json"))) {
    char json_msg[32];
    (void) clo_encode((clo[]) {clo_object({"message", clo_string("Hello, World!")})}, json_msg, sizeof(json_msg));
    reactor_server_ok(session, reactor_vector_string("application/json"), reactor_vector_string(json_msg));
    return REACTOR_OK;
  }
  else if (reactor_vector_equal(session->request->target, reactor_vector_string("/plaintext"))) {
    reactor_server_ok(session, reactor_vector_string("text/plain"), reactor_vector_string("Hello, World!"));
    return REACTOR_OK;
  }
  else {
    reactor_server_ok(session, reactor_vector_string("text/plain"), reactor_vector_string("Hello from libreactor!\n"));
    return REACTOR_OK;
  }
}


int main()
{
  reactor_server server;

  setup();
  reactor_construct();
  reactor_server_construct(&server, NULL, NULL);
  reactor_server_route(&server, tfb, NULL);
  (void) reactor_server_open(&server, "0.0.0.0", "8080");

  reactor_run();
  reactor_destruct();
}