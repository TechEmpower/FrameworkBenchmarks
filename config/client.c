/*
 * weighttp - a lightweight and simple webserver benchmarking tool
 *
 * Author:
 *     Copyright (c) 2009-2011 Thomas Porzelt
 *
 * License:
 *     MIT, see COPYING file
 */

#include "weighttp.h"

static uint8_t client_parse(Client *client, int size);
static void client_io_cb(struct ev_loop *loop, ev_io *w, int revents);
static void client_set_events(Client *client, int events);
/*
static void client_add_events(Client *client, int events);
static void client_rem_events(Client *client, int events);
static void client_add_events(Client *client, int events) {
  struct ev_loop *loop = client->worker->loop;
  ev_io *watcher = &client->sock_watcher;

  if ((watcher->events & events) == events)
    return;

  ev_io_stop(loop, watcher);
  ev_io_set(watcher, watcher->fd, watcher->events | events);
  ev_io_start(loop, watcher);
}

static void client_rem_events(Client *client, int events) {
  struct ev_loop *loop = client->worker->loop;
  ev_io *watcher = &client->sock_watcher;

  if (0 == (watcher->events & events))
    return;

  ev_io_stop(loop, watcher);
  ev_io_set(watcher, watcher->fd, watcher->events & ~events);
  ev_io_start(loop, watcher);
}
*/

static void client_set_events(Client *client, int events) {
  struct ev_loop *loop = client->worker->loop;
  ev_io *watcher = &client->sock_watcher;

  if (events == (watcher->events & (EV_READ | EV_WRITE)))
    return;

  ev_io_stop(loop, watcher);
  ev_io_set(watcher, watcher->fd, (watcher->events & ~(EV_READ | EV_WRITE)) | events);
  ev_io_start(loop, watcher);
}

Client *client_new(Worker *worker) {
  Client *client;

  client = W_MALLOC(Client, 1);
  client->state = CLIENT_START;
  client->worker = worker;
  client->sock_watcher.fd = -1;
  client->sock_watcher.data = client;
  client->content_length = -1;
  client->buffer_offset = 0;
  client->request_offset = 0;
  client->keepalive = client->worker->config->keep_alive;
  client->chunked = 0;
  client->chunk_size = -1;
  client->chunk_received = 0;

  return client;
}

void client_free(Client *client) {
  if (client->sock_watcher.fd != -1) {
    ev_io_stop(client->worker->loop, &client->sock_watcher);
    shutdown(client->sock_watcher.fd, SHUT_WR);
    close(client->sock_watcher.fd);
  }

  free(client);
}

static void client_reset(Client *client) {
  //printf("keep alive: %d\n", client->keepalive);
  if (!client->keepalive) {
    if (client->sock_watcher.fd != -1) {
      ev_io_stop(client->worker->loop, &client->sock_watcher);
      shutdown(client->sock_watcher.fd, SHUT_WR);
      close(client->sock_watcher.fd);
      client->sock_watcher.fd = -1;
    }

    client->state = CLIENT_START;
  } else {
    client_set_events(client, EV_WRITE);
    client->state = CLIENT_WRITING;
    client->worker->stats.req_started++;
  }

  client->parser_state = PARSER_START;
  client->buffer_offset = 0;
  client->parser_offset = 0;
  client->request_offset = 0;
  client->ts_start = 0;
  client->ts_end = 0;
  client->status_success = 0;
  client->success = 0;
  client->content_length = -1;
  client->bytes_received = 0;
  client->header_size = 0;
  client->keepalive = client->worker->config->keep_alive;
  client->chunked = 0;
  client->chunk_size = -1;
  client->chunk_received = 0;
}

static uint8_t client_connect(Client *client) {
  //printf("connecting...\n");
  start:

  if (-1 == connect(client->sock_watcher.fd, client->worker->config->saddr->ai_addr, client->worker->config->saddr->ai_addrlen)) {
    switch (errno) {
      case EINPROGRESS:
      case EALREADY:
        /* async connect now in progress */
        client->state = CLIENT_CONNECTING;
        return 1;
      case EISCONN:
        break;
      case EINTR:
        goto start;
      default:
      {
        strerror_r(errno, client->buffer, sizeof(client->buffer));
        W_ERROR("connect() failed: %s (%d)", client->buffer, errno);
        return 0;
      }
    }
  }

  /* successfully connected */
  client->state = CLIENT_WRITING;
  return 1;
}

static void client_io_cb(struct ev_loop *loop, ev_io *w, int revents) {
  Client *client = w->data;

  UNUSED(loop);
  UNUSED(revents);

  client_state_machine(client);
}

void client_state_machine(Client *client) {
  int r;
  Config *config = client->worker->config;

  start:
  //printf("state: %d\n", client->state);
  switch (client->state) {
    case CLIENT_START:
      client->worker->stats.req_started++;

      do {
        r = socket(config->saddr->ai_family, config->saddr->ai_socktype, config->saddr->ai_protocol);
      } while (-1 == r && errno == EINTR);

      if (-1 == r) {
        client->state = CLIENT_ERROR;
        strerror_r(errno, client->buffer, sizeof(client->buffer));
        W_ERROR("socket() failed: %s (%d)", client->buffer, errno);
        goto start;
      }

      /* set non-blocking */
      fcntl(r, F_SETFL, O_NONBLOCK | O_RDWR);

      ev_init(&client->sock_watcher, client_io_cb);
      ev_io_set(&client->sock_watcher, r, EV_WRITE);
      ev_io_start(client->worker->loop, &client->sock_watcher);

      if (!client_connect(client)) {
        client->state = CLIENT_ERROR;
        goto start;
      } else {
        client_set_events(client, EV_WRITE);
        return;
      }
    case CLIENT_CONNECTING:
      if (!client_connect(client)) {
        client->state = CLIENT_ERROR;
        goto start;
      }
    case CLIENT_WRITING:
      while (1) {
        r = write(client->sock_watcher.fd, &config->request[client->request_offset], config->request_size - client->request_offset);
        //printf("write(%d - %d = %d): %d\n", config->request_size, client->request_offset, config->request_size - client->request_offset, r);
        if (r == -1) {
          /* error */
          if (errno == EINTR)
            continue;
          strerror_r(errno, client->buffer, sizeof(client->buffer));
          W_ERROR("write() failed: %s (%d)", client->buffer, errno);
          client->state = CLIENT_ERROR;
          goto start;
        } else if (r != 0) {
          /* success */
          client->request_offset += r;
          if (client->request_offset == config->request_size) {
            /* whole request was sent, start reading */
            client->state = CLIENT_READING;
            client_set_events(client, EV_READ);
          }

          return;
        } else {
          /* disconnect */
          client->state = CLIENT_END;
          goto start;
        }
      }
    case CLIENT_READING:
      while (1) {
        r = read(client->sock_watcher.fd, &client->buffer[client->buffer_offset], sizeof(client->buffer) - client->buffer_offset - 1);
        
        //printf("read(): %d, offset was: %d\n", r, client->buffer_offset);
        if (r == -1) {
          /* error */
          if (errno == EINTR)
            continue;
          strerror_r(errno, client->buffer, sizeof(client->buffer));
          W_ERROR("read() failed: %s (%d)", client->buffer, errno);
          client->state = CLIENT_ERROR;
        } else if (r != 0) {
          /* success */
          client->bytes_received += r;
          client->buffer_offset += r;
          client->worker->stats.bytes_total += r;

          if (client->buffer_offset >= sizeof(client->buffer)) {
            /* too big response header */
            client->state = CLIENT_ERROR;
            break;
          }
          client->buffer[client->buffer_offset] = '\0';
          //printf("buffer:\n==========\n%s\n==========\n", client->buffer);
          if (!client_parse(client, r)) {
            client->state = CLIENT_ERROR;
            //printf("parser failed\n");
            break;
          } else {
            if (client->state == CLIENT_END)
              goto start;
            else
              return;
          }
        } else {
          /* disconnect */
          if (client->parser_state == PARSER_BODY && !client->keepalive && client->status_success
            && !client->chunked && client->content_length == -1) {
            client->success = 1;
            client->state = CLIENT_END;
          } else {
            client->state = CLIENT_ERROR;
          }

          goto start;
        }
      }

    case CLIENT_ERROR:
      //printf("client error\n");
      client->worker->stats.req_error++;
      client->keepalive = 0;
      client->success = 0;
      client->state = CLIENT_END;
    case CLIENT_END:
      /* update worker stats */
      client->worker->stats.req_done++;

      if (client->success) {
        client->worker->stats.req_success++;
        client->worker->stats.bytes_body += client->bytes_received - client->header_size;
      } else {
        client->worker->stats.req_failed++;
      }

      /* print progress every 10% done */
      if (client->worker->id == 1 && client->worker->stats.req_done % client->worker->progress_interval == 0) {
        printf("progress: %3d%% done\n",
          (int) (client->worker->stats.req_done * 100 / client->worker->stats.req_todo)
        );
      }

      if (client->worker->stats.req_started == client->worker->stats.req_todo) {
        /* this worker has started all requests */
        client->keepalive = 0;
        client_reset(client);

        if (client->worker->stats.req_done == client->worker->stats.req_todo) {
          /* this worker has finished all requests */
          ev_unref(client->worker->loop);
        }
      } else {
        client_reset(client);
        goto start;
      }
  }
}


static uint8_t client_parse(Client *client, int size) {
  char *end, *str;
  uint16_t status_code;
  const int MAX_BUFF = 2048;
  char* print_buff = malloc(MAX_BUFF);
  int buff_length = 0;

  switch (client->parser_state) {
    case PARSER_START:
      //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "parse (START):\n%s\n", &client->buffer[client->parser_offset]);

      /* look for HTTP/1.1 200 OK */
      if (client->buffer_offset < sizeof("HTTP/1.1 200\r\n")) {
        buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "error 1\n", client->buffer_offset);
        printf(print_buff);
        return 1;
      }

      if (strncmp(client->buffer, "HTTP/1.1 ", sizeof("HTTP/1.1 ")-1) != 0) {
        //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 1\n");
        printf(print_buff);
        return 0;
      }

      // now the status code
      status_code = 0;
      str = client->buffer + sizeof("HTTP/1.1 ")-1;
      for (end = str + 3; str != end; str++) {
        if (*str < '0' || *str > '9') {
          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 2\n");
          printf(print_buff);
          return 0;
        }

        status_code *= 10;
        status_code += *str - '0';
      }

      if (status_code >= 200 && status_code < 300) {
        client->worker->stats.req_2xx++;
        client->status_success = 1;
      } else if (status_code < 400) {
        client->worker->stats.req_3xx++;
        client->status_success = 1;
      } else if (status_code < 500) {
        client->worker->stats.req_4xx++;
      } else if (status_code < 600) {
        client->worker->stats.req_5xx++;
      } else {
        // invalid status code
        //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 3\n");
        printf(print_buff);
        return 0;
      }

      // look for next \r\n
      end = strchr(end, '\r');
      if (!end || *(end+1) != '\n') {
        //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 4\n");
        printf(print_buff);
        return 0;
      }

      client->parser_offset = end + 2 - client->buffer;
      client->parser_state = PARSER_HEADER;
    case PARSER_HEADER:
      //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "parse (HEADER)\n");
      /* look for Content-Length and Connection header */
      while (NULL != (end = strchr(&client->buffer[client->parser_offset], '\r'))) {
        if (*(end+1) != '\n') {
          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 5\n");
          printf(print_buff);
          return 0;
        }

        if (end == &client->buffer[client->parser_offset]) {
          /* body reached */
          client->parser_state = PARSER_BODY;
          client->header_size = end + 2 - client->buffer;
          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "body reached\n");

          return client_parse(client, size - client->header_size);
        }

        *end = '\0';
        str = &client->buffer[client->parser_offset];
        //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "checking header: '%s'\n", str);

        if (strncasecmp(str, "Content-Length: ", sizeof("Content-Length: ")-1) == 0) {
          /* content length header */
          client->content_length = str_to_uint64(str + sizeof("Content-Length: ") - 1);
        } else if (strncasecmp(str, "Connection: ", sizeof("Connection: ")-1) == 0) {
          /* connection header */
          str += sizeof("Connection: ") - 1;

          if (strncasecmp(str, "close", sizeof("close")-1) == 0)
            client->keepalive = 0;
          else if (strncasecmp(str, "keep-alive", sizeof("keep-alive")-1) == 0)
            client->keepalive = client->worker->config->keep_alive;
          else {
            //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 6\n");
            printf(print_buff);
            return 0;
          }
        } else if (strncasecmp(str, "Transfer-Encoding: ", sizeof("Transfer-Encoding: ")-1) == 0) {
          /* transfer encoding header */
          str += sizeof("Transfer-Encoding: ") - 1;

          if (strncasecmp(str, "chunked", sizeof("chunked")-1) == 0)
            client->chunked = 1;
          else {
            //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 7\n");
            printf(print_buff);
            return 0;
          }
        }


        if (*(end+2) == '\r' && *(end+3) == '\n') {
          /* body reached */
          client->parser_state = PARSER_BODY;
          client->header_size = end + 4 - client->buffer;
          client->parser_offset = client->header_size;
          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "body reached\n");

          return client_parse(client, size - client->header_size);
        }

        client->parser_offset = end - client->buffer + 2;
      }
      buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "error 2\n");
      printf(print_buff);
      return 1;
    case PARSER_BODY:
      //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "parse (BODY)\n");
      /* do nothing, just consume the data */
      /*buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "content-l: %"PRIu64", header: %d, recevied: %"PRIu64"\n",
      client->content_length, client->header_size, client->bytes_received);*/

      if (client->chunked) {
        int consume_max;

        str = &client->buffer[client->parser_offset];
        /*buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "parsing chunk: '%s'\n(%"PRIi64" received, %"PRIi64" size, %d parser offset)\n",
          str, client->chunk_received, client->chunk_size, client->parser_offset
        );*/

        if (client->chunk_size == -1) {
          /* read chunk size */
          client->chunk_size = 0;
          client->chunk_received = 0;
          end = str + size;

          for (; str < end; str++) {
            if (*str == ';' || *str == '\r')
              break;

            client->chunk_size *= 16;
            if (*str >= '0' && *str <= '9')
              client->chunk_size += *str - '0';
            else if (*str >= 'A' && *str <= 'Z')
              client->chunk_size += 10 + *str - 'A';
            else if (*str >= 'a' && *str <= 'z')
              client->chunk_size += 10 + *str - 'a';
            else {
              //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 8\n");
              printf(print_buff);
              return 0;
            }
          }

          str = strstr(str, "\r\n");
          if (!str) {
            //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 9\n");
            printf(print_buff);
            return 0;
          }
          str += 2;

          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "---------- chunk size: %"PRIi64", %d read, %d offset, data: '%s'\n", client->chunk_size, size, client->parser_offset, str);

          if (client->chunk_size == 0) {
            /* chunk of size 0 marks end of content body */
            client->state = CLIENT_END;
            client->success = client->status_success ? 1 : 0;
            if(client->success == 0) {
              buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "error 3\n");
            }
            printf(print_buff);
            return 1;
          }

          size -= str - &client->buffer[client->parser_offset];
          client->parser_offset = str - client->buffer;
        }

        /* consume chunk till chunk_size is reached */
        consume_max = client->chunk_size - client->chunk_received;

        if (size < consume_max)
          consume_max = size;

        client->chunk_received += consume_max;
        client->parser_offset += consume_max;

        //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "---------- chunk consuming: %d, received: %"PRIi64" of %"PRIi64", offset: %d\n", consume_max, client->chunk_received, client->chunk_size, client->parser_offset);

        if (client->chunk_received == client->chunk_size) {
          if (client->buffer[client->parser_offset] != '\r' || client->buffer[client->parser_offset+1] != '\n') {
            // TE: This was causing problems with resin, which was artificially slowing down the results of some java tests
            //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 10\n");
            //printf(print_buff);
            //return 0;
          }

          /* got whole chunk, next! */
          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "---------- got whole chunk!!\n");
          client->chunk_size = -1;
          client->chunk_received = 0;
          client->parser_offset += 2;
          consume_max += 2;

          /* there is stuff left to parse */
          if (size - consume_max > 0)
            return client_parse(client, size - consume_max);
        }

        client->parser_offset = 0;
        client->buffer_offset = 0;

        //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "error 4\n");
        printf(print_buff);
        return 1;
      } else {
        /* not chunked, just consume all data till content-length is reached */
        client->buffer_offset = 0;

        if (client->content_length == -1) {
          //buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "success 11\n");
          printf(print_buff);
          return 0;
        }

        if (client->bytes_received == (uint64_t) (client->header_size + client->content_length)) {
          /* full response received */
          client->state = CLIENT_END;
          client->success = client->status_success ? 1 : 0;
        }
      }
      buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "error 5\n");
      printf(print_buff);
      return 1;
  }
  buff_length += snprintf(print_buff+buff_length, MAX_BUFF-buff_length, "error 6\n");
  printf(print_buff);
  return 1;
}