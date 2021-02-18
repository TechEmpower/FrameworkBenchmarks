#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <err.h>
#include <sys/eventfd.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>

#include "helpers.h"


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
  int parent_eventfd;
  server s;

  // fork_workers() forks a separate child/worker process for each available cpu and returns an eventfd from the parent
  // The eventfd is used to signal the parent. This guarantees the forking order needed for REUSEPORT_CBPF to work well
  parent_eventfd = fork_workers();

  core_construct(NULL);
  server_construct(&s, server_handler, &s);
  server_open(&s, 0, 8080);
  enable_reuseport_cbpf(&s);

  // Signal the parent process so that it can proceed with the next fork
  eventfd_write(parent_eventfd, (eventfd_t) 1);
  close(parent_eventfd);

  core_loop(NULL);
  core_destruct(NULL);
}