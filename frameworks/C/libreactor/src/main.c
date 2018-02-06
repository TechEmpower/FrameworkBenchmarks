#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <signal.h>
#include <unistd.h>
#include <setjmp.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <sys/socket.h>
#include <sys/resource.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/queue.h>
#include <linux/sched.h>
#include <netdb.h>
#include <err.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>

#include "setup.h"

static char date[] = "Thu, 01 Jan 1970 00:00:00 GMT";

static int timer_event(void *state, int type, void *data)
{
  (void) state;
  (void) data;
  if (type != REACTOR_TIMER_EVENT_CALL)
    err(1, "timer");

  reactor_http_date(date);
  return REACTOR_OK;
}

static int plaintext(reactor_http *http)
{
  char content_length[11], *body = "Hello, World!";
  size_t size;

  size = strlen(body);
  reactor_util_u32toa(size, content_length);
  reactor_http_send_response(http, (reactor_http_response[]){{
        .version = 1,
	.status = 200,
	.reason = {"OK", 2},
	.headers = 4,
        .header[0] = {{"Server", 6}, {"libreactor", 10}},
        .header[1] = {{"Date", 4}, {date, strlen(date)}},
	.header[2] = {{"Content-Type", 12}, {"text/plain", 10}},
	.header[3] = {{"Content-Length", 14}, {content_length, strlen(content_length)}},
	.body = {body, size}
      }});
  return REACTOR_OK;
}

static int json(reactor_http *http)
{
  char body[4096], content_length[11];
  size_t size;
  
  (void) clo_encode((clo[]) {clo_object({"message", clo_string("Hello, World!")})}, body, sizeof(body));
  size = strlen(body);
  reactor_util_u32toa(size, content_length);
  reactor_http_send_response(http, (reactor_http_response[]){{
        .version = 1,
	.status = 200,
	.reason = {"OK", 2},
	.headers = 4,
        .header[0] = {{"Server", 6}, {"libreactor", 10}},
        .header[1] = {{"Date", 4}, {date, strlen(date)}},
	.header[2] = {{"Content-Type", 12}, {"application/json", 16}},
	.header[3] = {{"Content-Length", 14}, {content_length, strlen(content_length)}},
	.body = {body, size}
      }});
  return REACTOR_OK;
}

int http_event(void *state, int type, void *data)
{
  reactor_http *http = state;
  reactor_http_request *request;

  if (reactor_unlikely(type != REACTOR_HTTP_EVENT_REQUEST))
    {
      reactor_http_close(http);
      free(http);
      return REACTOR_ABORT;
    }
  request = data;
  
  if (reactor_http_header_value(&request->path, "/plaintext"))
    return plaintext(http);
  else if (reactor_http_header_value(&request->path, "/json"))
    return json(http);
  else
    {
      reactor_http_send_response(http, (reactor_http_response[]){{
	    .version = 1,
	    .status = 404,
	    .reason = {"Not Found", 9},
	    .headers = 3,
            .header[0] = {{"Server", 6}, {"libreactor", 10}},
            .header[1] = {{"Date", 4}, {date, strlen(date)}},
	    .header[2] = {{"Content-Length", 14}, {"0", 1}},
	    .body = {NULL, 0}
	  }});
      return REACTOR_OK;
    }
}

static int tcp_event(void *state, int type, void *data)
{
  reactor_http *http;

  if (type != REACTOR_TCP_EVENT_ACCEPT)
    err(1, "tcp");

  http = malloc(sizeof *http);
  if (!http)
    abort();
  (void) reactor_http_open(http, http_event, http, *(int *) data, REACTOR_HTTP_FLAG_SERVER);
  return REACTOR_OK;
}

int main()
{
  reactor_timer timer;
  reactor_tcp tcp;

  setup(1, 0);  
  (void) reactor_core_construct();
  (void) reactor_timer_open(&timer, timer_event, &timer, 1, 1000000000);
  (void) reactor_tcp_open(&tcp, tcp_event, &tcp, "0.0.0.0", "8080", REACTOR_TCP_FLAG_SERVER);
  (void) reactor_core_run();
  reactor_core_destruct();
}
