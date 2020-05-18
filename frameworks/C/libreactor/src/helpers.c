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

static char date_header[] = "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n";
static size_t date_header_size = sizeof(date_header) - 1;


// Updates the date string
void update_date()
{
  static const char *days[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
  static const char *months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

  time_t t;
  struct tm tm;

  (void) time(&t);
  (void) gmtime_r(&t, &tm);
  (void) strftime(date_header, date_header_size, "Date: ---, %d --- %Y %H:%M:%S GMT\r\n", &tm);
  memcpy(date_header + 6, days[tm.tm_wday], 3);
  memcpy(date_header + 14, months[tm.tm_mon], 3);
}

// Copies the date string to the preamble
void copy_date(char *target, size_t position)
{
  memcpy(target + position, date_header, date_header_size);
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
  (void) getaddrinfo(node, service, &hints, &ai);

  if (!ai)
    err(1, "error resolving address");

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