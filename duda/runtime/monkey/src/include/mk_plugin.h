/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU Lesser General Public  License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef MK_PLUGIN_H
#define MK_PLUGIN_H

#include "monkey.h"
#include "mk_config.h"
#include "mk_request.h"
#include "mk_memory.h"
#include "mk_iov.h"
#include "mk_socket.h"
#include "mk_epoll.h"
#include "mk_header.h"
#include "mk_http_status.h"
#include "mk_utils.h"
#include "mk_string.h"
#include "mk_list.h"
#include "mk_info.h"

#define MK_PLUGIN_LOAD "plugins.load"

#define MK_PLUGIN_ERROR -1      /* plugin execution error */
#define MK_PLUGIN_

/* Plugin: Core types */
#define MK_PLUGIN_CORE_PRCTX (1)
#define MK_PLUGIN_CORE_THCTX (2)

/* Plugin: Stages */
#define MK_PLUGIN_STAGE_10 (4)    /* Connection just accept()ed */
#define MK_PLUGIN_STAGE_20 (8)    /* HTTP Request arrived */
#define MK_PLUGIN_STAGE_30 (16)   /* Object handler  */
#define MK_PLUGIN_STAGE_40 (32)   /* Content served */
#define MK_PLUGIN_STAGE_50 (64)   /* Conection ended */

/* Plugin: Network type */
#define MK_PLUGIN_NETWORK_IO (128)
#define MK_PLUGIN_NETWORK_IP (256)

/* Return values */
#define MK_PLUGIN_RET_NOT_ME -1
#define MK_PLUGIN_RET_CONTINUE 100
#define MK_PLUGIN_RET_END 200
#define MK_PLUGIN_RET_CLOSE_CONX 300
#define MK_PLUGIN_HEADER_EXTRA_ROWS  18

/*
 * Event return values
 * -------------------
 * Any plugin can hook to any socket event, when a worker thread receives
 * a socket event through epoll(), it will check first the plugins hooks
 * before return the control to Monkey core.
 */

 /* The plugin request to the caller to continue invoking next plugins */
#define MK_PLUGIN_RET_EVENT_NEXT -300

/* The plugin has taken some action and no other plugin should go
 * over the event in question, return as soon as possible
 */
#define MK_PLUGIN_RET_EVENT_OWNED -400

/* The plugin request to finalize the session request */
#define MK_PLUGIN_RET_EVENT_CLOSE -500

/* The plugin request to the caller skip event hooks */
#define MK_PLUGIN_RET_EVENT_CONTINUE -600

/* Contexts: process/thread */
struct plugin_core
{
    int  (*prctx) (struct server_config *);
    void (*thctx) ();
};

struct plugin_stage
{
    int (*s10) (int, struct sched_connection *);
    int (*s20) (struct client_session *, struct session_request *);
    int (*s30) (struct plugin *, struct client_session *, struct session_request *);
    int (*s40) (struct client_session *, struct session_request *);
    int (*s50) (int);
};

struct plugin_network_io
{
    int (*accept) (int);
    int (*read) (int, void *, int);
    int (*write) (int, const void *, size_t);
    int (*writev) (int, struct mk_iov *);
    int (*close) (int);
    int (*connect) (char *, int);
    int (*send_file) (int, int, off_t *, size_t);
    int (*create_socket) (int, int, int);
    int (*bind) (int, const struct sockaddr *addr, socklen_t, int);
    int (*server) (int, char *);
};

struct plugin
{
    char *shortname;
    char *name;
    char *version;
    char *path;
    void *handler;
    unsigned int hooks;

    /* Mandatory calls */
    int (*init) (void *, char *);
    int  (*exit) ();

    /* Hook functions by type */
    struct plugin_core core;
    struct plugin_stage stage;
    struct plugin_network_io net_io;

    /* Epoll Events */
    int (*event_read) (int);
    int (*event_write) (int);
    int (*event_error) (int);
    int (*event_close) (int);
    int (*event_timeout) (int);

    /* Each plugin has a thread key for it's global data */
    pthread_key_t *thread_key;

    /* Next! */
    struct mk_list _head;
};


/* Multiple plugins can work on multiple stages, we don't want
 * Monkey be comparing each plugin looking for a specific stage,
 * so we create a Map of direct stage calls
 */
struct plugin_stagem
{
    struct plugin *p;
    struct plugin_stagem *next;
};

struct plugin_stagemap
{
    struct plugin_stagem *stage_10;
    struct plugin_stagem *stage_15;
    struct plugin_stagem *stage_20;
    struct plugin_stagem *stage_30;
    struct plugin_stagem *stage_40;
    struct plugin_stagem *stage_50;
};

/* Network map calls */
extern struct plugin_network_io *plg_netiomap;

/* API functions exported to plugins */
struct plugin_api
{
    /* socket functions */
    int (*socket_cork_flag) (int, int);
    int (*socket_reset) (int);
    int (*socket_set_tcp_fastopen) (int);
    int (*socket_set_tcp_nodelay) (int);
    int (*socket_connect) (char *, int);
    int (*socket_set_nonblocking) (int);
    int (*socket_create) ();
    int (*socket_close) (int);
    int (*socket_sendv) (int, struct mk_iov *);
    int (*socket_send) (int, const void *, size_t);
    int (*socket_read) (int, void *, int);
    int (*socket_send_file) (int, int, off_t *, size_t);
    int (*socket_ip_str) (int, char **, int, unsigned long *);

    struct server_config *config;
    struct mk_list *plugins;
    struct sched_list_node *sched_list;

    /* Error helper */
    void (*_error) (int, const char *, ...) PRINTF_WARNINGS(2,3);

    /* HTTP request function */
    int   (*http_request_end) (int);
    int   (*http_request_error) (int, struct client_session *, struct session_request *);

    /* memory functions */
    void *(*mem_alloc) (const size_t size);
    void *(*mem_alloc_z) (const size_t size);
    void *(*mem_realloc) (void *, const size_t size);
    void  (*mem_free) (void *);
    void  (*pointer_set) (mk_pointer *, char *);
    void  (*pointer_print) (mk_pointer);
    char *(*pointer_to_buf) (mk_pointer);

    /* string functions */
    int   (*str_itop) (int, mk_pointer *);
    int   (*str_search) (const char *, const char *, int);
    int   (*str_search_n) (const char *, const char *, int, int);
    char *(*str_build) (char **, unsigned long *, const char *, ...) PRINTF_WARNINGS(3,4);
    char *(*str_dup) (const char *);
    char *(*str_copy_substr) (const char *, int, int);
    struct mk_list *(*str_split_line) (const char *);
    void  (*str_split_free) (struct mk_list *);

    /* file functions */
    char *(*file_to_buffer) (const char *);
    int  (*file_get_info) (const char *, struct file_info *);

    /* header */
    int  (*header_send) (int, struct client_session *, struct session_request *);
    mk_pointer (*header_get) (struct headers_toc *, const char *key_name, int key_len);
    int  (*header_add) (struct session_request *, char *row, int len);
    void (*header_set_http_status) (struct session_request *, int);

    /* iov functions */
    struct mk_iov *(*iov_create) (int, int);
    int (*iov_realloc) (struct mk_iov *, int);
    void (*iov_free) (struct mk_iov *);
    void (*iov_free_marked) (struct mk_iov *);
    int (*iov_add_entry) (struct mk_iov *, char *, int, mk_pointer, int);
    int (*iov_set_entry) (struct mk_iov *, char *, int, int, int);
    ssize_t (*iov_send) (int, struct mk_iov *);
    void (*iov_print) (struct mk_iov *);

    /* plugin functions */
    void *(*plugin_load_symbol) (void *, const char *);

    /* epoll functions */
    void *(*epoll_init) (int, int);
    int   (*epoll_create) (int);
    int   (*epoll_add) (int, int, int, unsigned int);
    int   (*epoll_del) (int, int);
    int   (*epoll_change_mode) (int, int, int, unsigned int);
    struct epoll_state *(*epoll_state_get) (int);

    /* Mime type */
    struct mimetype *(*mimetype_lookup) (char *);

    /* red-black tree */
    void (*rb_insert_color) (struct rb_node *, struct rb_root *);
    void (*rb_erase) (struct rb_node *, struct rb_root *);
    void (*rb_link_node) (struct rb_node *, struct rb_node *, struct rb_node **);

    /* configuration reader functions */
    struct mk_config *(*config_create) (const char *);
    void (*config_free) (struct mk_config *);
    struct mk_config_section *(*config_section_get) (struct mk_config *,
                                                     const char *);
    void *(*config_section_getval) (struct mk_config_section *, char *, int);


    /* Scheduler */
    int (*sched_remove_client) (int);
    struct sched_connection *(*sched_get_connection) (struct sched_list_node *,
                                                      int);
    struct sched_list_node *(*sched_worker_info)();

    /* worker's functions */
    pthread_t (*worker_spawn) (void (*func) (void *), void *);
    int (*worker_rename) (const char *);

    /* event's functions */
    int (*event_add) (int, int, struct plugin *, unsigned int);
    int (*event_del) (int);
    struct plugin_event *(*event_get) (int);

    int (*event_socket_change_mode) (int, int, unsigned int);

    /* Time utils functions */
    int (*time_unix) ();
    int (*time_to_gmt) (char **, time_t);
    mk_pointer *(*time_human) ();

#ifdef TRACE
    void (*trace)(const char *, int, const char *, char *, int, const char *, ...);
    int (*errno_print) (int);
#endif
    void (*stacktrace)(void);

};

extern struct plugin_api *api;

/* Plugin events thread key */
extern pthread_key_t mk_plugin_event_k;

struct plugin_event
{
    int socket;

    struct plugin *handler;

    struct mk_list _head;
};

struct plugin_info {
    const char *shortname;
    const char *name;
    const char *version;
    unsigned int hooks;
};

void mk_plugin_init();
void mk_plugin_read_config();
void mk_plugin_exit_all();

void mk_plugin_event_init_list();

int mk_plugin_stage_run(unsigned int stage,
                        unsigned int socket,
                        struct sched_connection *conx,
                        struct client_session *cs, struct session_request *sr);

void mk_plugin_core_process();
void mk_plugin_core_thread();

void mk_plugin_preworker_calls();

/* Plugins events interface */
int mk_plugin_event_add(int socket, int mode,
                        struct plugin *handler,
                        unsigned int behavior);
int mk_plugin_event_del(int socket);
struct plugin_event *mk_plugin_event_get(int socket);

int mk_plugin_event_socket_change_mode(int socket, int mode, unsigned int behavior);

/* Plugins event handlers */
int mk_plugin_event_read(int socket);
int mk_plugin_event_write(int socket);
int mk_plugin_event_error(int socket);
int mk_plugin_event_close(int socket);
int mk_plugin_event_timeout(int socket);

void *mk_plugin_load(const char *path);
void mk_plugin_register_to(struct plugin **st, struct plugin *p);
void *mk_plugin_load_symbol(void *handler, const char *symbol);
int mk_plugin_http_request_end(int socket);

/* Register functions */
struct plugin *mk_plugin_register(struct plugin *p);
void mk_plugin_unregister(struct plugin *p);

struct plugin *mk_plugin_alloc(void *handler, const char *path);
void mk_plugin_free(struct plugin *p);

int mk_plugin_time_now_unix();
mk_pointer *mk_plugin_time_now_human();

int mk_plugin_sched_remove_client(int socket);

int mk_plugin_header_add(struct session_request *sr, char *row, int len);
int mk_plugin_header_get(struct session_request *sr,
                         mk_pointer query,
                         mk_pointer *result);

#endif
