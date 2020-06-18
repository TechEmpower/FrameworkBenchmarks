#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <netdb.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <time.h>
#include <err.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/epoll.h>

#include <dynamic.h>
#include <reactor.h>
#include <clo.h>

// Returns the full header and trailing \r\n
reactor_vector http_date_header(int update)
{
  time_t t;
  struct tm tm;
  static const char *days[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
  static const char *months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
  static __thread char date_header[38] = "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n";

  if (update)
    {
      (void) time(&t);
      (void) gmtime_r(&t, &tm);
      (void) strftime(date_header, 38, "Date: ---, %d --- %Y %H:%M:%S GMT\r\n", &tm);
      memcpy(date_header + 6, days[tm.tm_wday], 3);
      memcpy(date_header + 14, months[tm.tm_mon], 3);
    }

  return (reactor_vector) {date_header, 37};
}

// Returns the full header and trailing \r\n
// Also includes the final \r\n separating the headers from response body
reactor_vector http_content_length_header(uint32_t n)
{
  static __thread char header[32] = "Content-Length: "; // full 32 elements still allocated
  size_t length = reactor_utility_u32len(n);

  reactor_utility_u32sprint(n, header + length + 16);
  memcpy(header + length + 16, "\r\n\r\n", 4);

  return (reactor_vector){header, length + 16 + 4};
}

// memcpy the response directly to the output buffer and adjust the buffer size accordingly
void write_response(reactor_stream *stream, reactor_vector preamble, reactor_vector body)
{
  char *output_stream_ptr;
  reactor_vector date_header = http_date_header(0); // includes header name and \r\n
  reactor_vector content_length_header = http_content_length_header(body.size); // includes header name and \r\n\r\n
  size_t response_size = preamble.size + date_header.size + content_length_header.size + body.size;

  buffer_reserve(&stream->output, response_size);
  output_stream_ptr = (char *) buffer_data(&stream->output);
  memcpy(output_stream_ptr, preamble.base, preamble.size);
  memcpy(output_stream_ptr + preamble.size, date_header.base, date_header.size);
  memcpy(output_stream_ptr + preamble.size + date_header.size, content_length_header.base, content_length_header.size);
  memcpy(output_stream_ptr + preamble.size + date_header.size + content_length_header.size, body.base, body.size);
  stream->output.size += response_size;
}


// Custom version of reactor_net_bind that doesn't use reactor_resolver/reactor_pool to resolve the address
// We just call getaddrinfo here directly instead. It is called at start we don't need to worry about it blocking
// By not using reactor_net_bind we can avoid reactor_pool altogether and compile without -pthread. This seems to
// provide some additional performance
reactor_status custom_reactor_net_bind(reactor_net *net, char *node, char *service)
{
  int e, fileno;
  struct addrinfo *ai;
  struct addrinfo hints;

  if (reactor_fd_active(&net->fd))
    return REACTOR_ERROR;

  net->options |= REACTOR_NET_OPTION_PASSIVE;

  hints = (struct addrinfo) {.ai_family = 0, .ai_socktype = SOCK_STREAM, .ai_flags = AI_PASSIVE};
  e = getaddrinfo(node, service, &hints, &ai);

  if (e)
    err(1, "unable to resolve %s:%s %s\n", node, service, gai_strerror(e));

  fileno = socket(ai->ai_family, SOCK_STREAM | SOCK_NONBLOCK, 0);
  reactor_assert_int_not_equal(fileno, -1);

  if (net->options & REACTOR_NET_OPTION_PASSIVE)
    {
      if (net->options & REACTOR_NET_OPTION_REUSEPORT)
        (void) setsockopt(fileno, SOL_SOCKET, SO_REUSEPORT, (int[]){1}, sizeof(int));
      if (net->options & REACTOR_NET_OPTION_REUSEADDR)
        (void) setsockopt(fileno, SOL_SOCKET, SO_REUSEADDR, (int[]){1}, sizeof(int));

      e = bind(fileno, ai->ai_addr, ai->ai_addrlen);
      if (e == -1)
        {
          (void) close(fileno);
          err(1, "error binding socket");
        }

      e = listen(fileno, INT_MAX);
      reactor_assert_int_not_equal(e, -1);
      reactor_fd_open(&net->fd, fileno, EPOLLIN | EPOLLET);
    }
  else
    {
     e = connect(fileno, ai->ai_addr, ai->ai_addrlen);
      if (e == -1 && errno != EINPROGRESS)
        {
          (void) close(fileno);
          err(1, "error connecting socket");
        }

      reactor_fd_open(&net->fd, fileno, EPOLLOUT | EPOLLET);
    }

  freeaddrinfo(ai);

  return REACTOR_OK;
}