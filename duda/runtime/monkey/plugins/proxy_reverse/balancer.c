/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2013, Nikola Nikov
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301  USA.
 */

#include <regex.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>

#include "types.h"
#include "config.h"

#define SERVER_KEY_SIZE_LIMIT (127 + 1 + 5)     /* domain:port */

static unsigned next = 0;

static unsigned next_shared = 0;
static pthread_mutex_t next_mutex = PTHREAD_MUTEX_INITIALIZER;

struct dict servers;
static pthread_mutex_t servers_mutex = PTHREAD_MUTEX_INITIALIZER;

static unsigned highavail_count;
static time_t highavail_timeout;
static struct string stats_url;

struct server
{
    unsigned connections;
    unsigned offline_count;
    time_t offline_last;
};

char *_format_uint_nofill(char *restrict buffer, uint64_t number,
                          uint8_t base);

// If number can't fit in length bytes, the behavior is undefined.
char *_format_uint_fill(char *restrict buffer, uint64_t number, uint8_t base,
                        uint16_t length, char fill);

#define _VA_ARGS_EMPTY(...) (sizeof(#__VA_ARGS__) == 1)

#define _ARGS5(func, a0, a1, a2, a3, a4, ...)   (func)(a0, a1, a2, a3, a4)

// __VA_ARGS__ +0 is used below to prevent compiler error about empty argument

// Add 3rd argument to 2 argument calls.
#define format_uint(buffer, number, ...) _format_uint_((buffer), (number), _VA_ARGS_EMPTY(__VA_ARGS__) ? 10 : __VA_ARGS__ +0)
// Call function depending on whether fill length is specified.
#define _format_uint_(buffer, number, base, ...) (_VA_ARGS_EMPTY(__VA_ARGS__) ? \
    _format_uint_nofill(buffer, number, base) : \
    _ARGS5(_format_uint_fill, buffer, number, base, __VA_ARGS__ +0, ' ') \
)

static const unsigned char digits[64] =
    "0123456789abcdefghijklmnopqrstuvwxyz";

// Writes string representation in base base of number in buffer.
// WARNING: buffer and number must be lvalues. TODO: explain what should base be
// _next specifies how to iterate buffer after each character (it's either ++ or -- ).
#define _format_digits(buffer, number, base, _next) do \
    { \
        do *(buffer)_next = digits[(size_t)((number) % (base))]; \
        while ((number) /= (base)); \
    } while (false)

#define _format_uint_nofill_internal(buffer, number, base) do \
    { \
        char *start = (buffer); \
        _format_digits((buffer), (number), (base), ++); \
        char *end = (buffer)--; \
        char swap; \
        /* reverse digits to obtain the number */ \
        while (start < (buffer)) \
        { \
            swap = *(buffer); \
            *(buffer) = *start; \
            *start = swap; \
            ++start, --(buffer); \
        } \
        return end; \
    } while (false)

char *_format_uint_nofill(char *buffer, uint64_t number, uint8_t base)
{
    _format_uint_nofill_internal(buffer, number, base);
}

// If number can't fit in length bytes, the behavior is undefined.
char *_format_uint_fill(char *buffer, uint64_t number, uint8_t base,
                        uint16_t length, char fill)
{
    char *end = buffer + length, *position = end - 1;
    _format_digits(position, number, base, --);
    while (position >= buffer)
        *position-- = fill;
    return end;
}

uint16_t format_uint_length(uint64_t number, uint8_t base)
{
    uint16_t length = 1;
    while (number /= base)
        ++length;
    return length;
}

static char *format_bytes(char *buffer, const char *bytes, size_t size)
{
    memcpy(buffer, bytes, size);
    return buffer + size;
}

static int key_init(struct string *key,
                    const struct proxy_server_entry *entry)
{
    size_t length = format_uint_length(entry->port, 10);
    key->length = strlen(entry->hostname);
    if ((key->length + 1 + length) > SERVER_KEY_SIZE_LIMIT) {
        return -1;              /* invalid server entry */
    }

    memcpy(key->data, entry->hostname, key->length);
    key->data[key->length++] = ':';
    format_uint(key->data + key->length, entry->port, 10, length);
    key->length += length;

    return 0;
}

int proxy_balance_init(const struct proxy_entry_array *config)
{
    if (!dict_init(&servers, DICT_SIZE_BASE)) {
        return ERROR_MEMORY;
    }

    size_t i, j;
    char buffer[SERVER_KEY_SIZE_LIMIT];
    struct string key;
    struct server *value;
    int status;

    key.data = buffer;

    highavail_count = config->entry[0].count;
    highavail_timeout = config->entry[0].timeout;
    stats_url.data = config->entry[0].stats_url;
    stats_url.length = strlen(stats_url.data);

    /* Add entry for each slave server in the servers dictionary. */
    for (i = 0; i < config->length; i++) {
        for (j = 0; j < config->entry[i].server_list->length; j++) {
            if (key_init(&key, config->entry[i].server_list->entry + j) < 0) {
                return -2;
            }

            value = mk_api->mem_alloc(sizeof(struct server));
            if (!value) {
                return ERROR_MEMORY;    /* memory error */
            }
            value->connections = 0;
            value->offline_count = 0;
            value->offline_last = 0;

            status = dict_add(&servers, &key, value);   /* dict_add will auto reject all repeated entries */
            if (status == ERROR_MEMORY) {
                return ERROR_MEMORY;
            }
        }
    }

    /*
       From here on, the only allowed modification of servers is to change the value of an item.
       This allows certain performance optimizations to be done.
     */

    return 0;
}

static int balance_connect(const struct proxy_server_entry *entry)
{
    int fd;

    char buffer[SERVER_KEY_SIZE_LIMIT];
    struct string key;

    volatile struct server *info = 0;

    time_t now = time(0);

    if (highavail_timeout) {
        key.data = buffer;
        if (key_init(&key, entry) < 0) {
            return -1;
        }

        info = dict_get(&servers, &key);

        pthread_mutex_lock(&servers_mutex);
        bool cancel = (((now - info->offline_last) < highavail_timeout)
                       && (info->offline_count >= highavail_count));
        pthread_mutex_unlock(&servers_mutex);
        if (cancel) {
            return -1;
        }
    }

    /*
       Try to connect. If the connection succeeds, make sure server parameters indicate that it's available.
       Otherwise update server parameters to indicate the current state of the server.
     */
    fd = mk_api->socket_connect(entry->hostname, entry->port);
    if (fd >= 0) {
        if (highavail_timeout) {
            pthread_mutex_lock(&servers_mutex);
            info->offline_count = 0;
            info->offline_last = 0;
            pthread_mutex_unlock(&servers_mutex);
        }

        mk_api->socket_set_nonblocking(fd);
    }
    else if (highavail_timeout) {
        pthread_mutex_lock(&servers_mutex);
        info->offline_count += 1;
        info->offline_last = now;
        pthread_mutex_unlock(&servers_mutex);
    }

    return fd;
}

/* Simple, non-fair load balancer with almost no overhead. */
int proxy_balance_naive(const struct proxy_server_entry_array *server_list,
                        unsigned seed)
{
    size_t index;
    int fd;

    for (index = 0; index < server_list->length; ++index) {
        fd = balance_connect(server_list->entry +
                             ((index + seed) % server_list->length));
        if (fd >= 0) {
            return fd;
        }
    }

    return -1;
}

/* Simple, fast load balancer based on request IP address. Non-fair balancing is possible in special circumstances. */
int proxy_balance_hash(const struct proxy_server_entry_array *server_list,
                       int sock)
{
    struct sockaddr_storage source;
    socklen_t length;
    in_addr_t address;
    char *p = (char *) &address;

    /* Retrieve socket source address struct and extract the source IP address from it. */
    length = sizeof(source);
    if (getpeername(sock, (struct sockaddr *) &source, &length) < 0) {
        return -1;
    }
    switch (source.ss_family) {
    case AF_INET:
        memcpy(p, (unsigned char *) &((struct sockaddr_in *) &source)->sin_addr, sizeof(in_addr_t));    /* we can do this because in_addr_t is stored in big endian */
        break;
    case AF_INET6:
        memcpy(p, ((struct sockaddr_in6 *) &source)->sin6_addr.s6_addr + 12, sizeof(in_addr_t));        /* the last 32 bits will suffice for calculating the hash */
        break;
    default:
        return -1;              /* invalid address */
    }

    return balance_connect(server_list->entry +
                           (address % server_list->length));
}

/* Simple load balancer with almost no overhead. Race conditions are possible under heavy load, but they will just lead to unfair sharing of the load. */
int proxy_balance_rr_lockless(const struct proxy_server_entry_array
                              *server_list)
{
    size_t index, from, to;
    int fd = -1;

    for (from = next, to = from + server_list->length; from < to; ++from) {
        index = from % server_list->length;
        fd = balance_connect(server_list->entry + index);
        if (fd >= 0) {
            next = index + 1;   /* remember which server handled the request */
            break;
        }
    }

    return fd;
}

/* Simple load balancer. Race conditions are prevented with mutexes. This adds significant overhead under heavy load. */
int proxy_balance_rr_locking(const struct proxy_server_entry_array
                             *server_list)
{
    size_t index, from, to;
    int fd = -1;

    pthread_mutex_lock(&next_mutex);

    for (from = next_shared, to = from + server_list->length; from < to;
         ++from) {
        index = from % server_list->length;
        fd = balance_connect(server_list->entry + index);
        if (fd >= 0) {
            next_shared = index + 1;    /* remember which server handled the request */
            break;
        }
    }

    pthread_mutex_unlock(&next_mutex);
    return fd;
}

/* Ensures equal load in most use cases. All servers are traversed to find the one with least connections. This adds significant overhead. */
int proxy_balance_leastconnections(const struct proxy_server_entry_array
                                   *server_list, void **connection)
{
    int fd;

    size_t index, index_min;
    struct server *info, *info_min;

    char buffer[SERVER_KEY_SIZE_LIMIT];
    struct string key;

    key.data = buffer;

    if (key_init(&key, server_list->entry) < 0) {
        return -2;
    }
    info_min = dict_get(&servers, &key);
    index_min = 0;

    pthread_mutex_lock(&servers_mutex);

    /* Find the slave server with the least number of connections. */
    for (index = 1; index < server_list->length; ++index) {
        if (key_init(&key, server_list->entry + index) < 0) {
            return -2;
        }

        info = dict_get(&servers, &key);

        if (info->connections < info_min->connections) {
            info_min = info;
            index_min = index;
        }
    }

    fd = balance_connect(server_list->entry + index_min);
    if (fd >= 0)
        info_min->connections += 1;

    pthread_mutex_unlock(&servers_mutex);

    key_init(&key, server_list->entry + index_min);
    *connection = string_alloc(key.data, key.length);

    return fd;
}

void proxy_balance_close(void *connection)
{
    struct server *info = dict_get(&servers, connection);

    pthread_mutex_lock(&servers_mutex);
    info->connections -= 1;
    pthread_mutex_unlock(&servers_mutex);

    free(connection);
}

struct string *proxy_balance_generate_statistics(struct session_request *sr)
{
    size_t length = 39;
    struct dict_iterator it;
    const struct dict_item *item;
    struct server *value;
    struct string *html;

    if (!stats_url.data) {
        return 0;
    }
    if (sr->uri_processed.len != stats_url.length) {
        return 0;
    }
    if (memcmp(sr->uri_processed.data, stats_url.data, stats_url.length)) {
        return 0;
    }

    html = mk_api->mem_alloc(sizeof(struct string));
    //Calculating the length

    for (item = dict_first(&it, &servers); item;
         item = dict_next(&it, &servers)) {
        length +=
            item->key_size + sizeof("<br><b></b><br>") - 1 +
            sizeof("Connections:<br>") - 1 + 10 +
            sizeof("Offline Count:<br>") - 1 + 10 +
            sizeof("Offline Last Check:<br>") - 1 + 10 + 1;
    }

    html->data = mk_api->mem_alloc(length * sizeof(char));

#define STR(s) (s), sizeof(s) - 1

    char *start = format_bytes(html->data, STR("<html><head></head><body>"));

    for (item = dict_first(&it, &servers); item;
         item = dict_next(&it, &servers)) {
        value = item->value;

        start = format_bytes(start, STR("<br /><b>"));
        start = format_bytes(start, item->key_data, item->key_size);
        start = format_bytes(start, STR("</b><br />"));

        start = format_bytes(start, STR("Connections:"));
        start = format_uint(start, value->connections);
        start = format_bytes(start, STR("<br />"));

        start = format_bytes(start, STR("Offline Count:"));
        start = format_uint(start, value->offline_count);
        start = format_bytes(start, STR("<br />"));

        start = format_bytes(start, STR("Offline Last Check:"));
        start = format_uint(start, value->offline_last);
        start = format_bytes(start, STR("<br />"));
    }

    start = format_bytes(start, STR("</body></html>"));

#undef STR

    html->length = start - html->data;
    html->data[html->length] = 0;

    return html;
}
