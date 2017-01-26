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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <pthread.h>

#ifdef LINUX_TRACE
#define TRACEPOINT_CREATE_PROBES
#define TRACEPOINT_DEFINE
#include "mk_linuxtrace.h"
#endif

#include "monkey.h"
#include "mk_socket.h"
#include "mk_user.h"
#include "mk_signals.h"
#include "mk_clock.h"
#include "mk_cache.h"
#include "mk_server.h"
#include "mk_plugin.h"
#include "mk_macros.h"
#include "mk_env.h"
#include "mk_http.h"
#include "mk_utils.h"

#if defined(__DATE__) && defined(__TIME__)
static const char MONKEY_BUILT[] = __DATE__ " " __TIME__;
#else
static const char MONKEY_BUILT[] = "Unknown";
#endif

const mk_pointer mk_monkey_protocol = mk_pointer_init(MK_HTTP_PROTOCOL_11_STR);
gid_t EGID;
gid_t EUID;

void mk_thread_keys_init(void)
{
    /* Create thread keys */
    pthread_key_create(&worker_sched_node, NULL);
    pthread_key_create(&request_list, NULL);
    pthread_key_create(&mk_cache_iov_header, NULL);
    pthread_key_create(&mk_cache_header_lm, NULL);
    pthread_key_create(&mk_cache_header_cl, NULL);
    pthread_key_create(&mk_cache_header_ka, NULL);
    pthread_key_create(&mk_cache_header_ka_max, NULL);
    pthread_key_create(&mk_cache_utils_gmtime, NULL);
    pthread_key_create(&mk_cache_utils_gmt_text, NULL);
    pthread_key_create(&mk_plugin_event_k, NULL);
    pthread_key_create(&mk_utils_error_key, NULL);
}

#ifndef SHAREDLIB
void mk_details(void)
{
    printf("* Process ID is %i", getpid());
    printf("\n* Server socket listening on Port %i", config->serverport);
    printf("\n* %i threads, %i client connections per thread, total %i",
           config->workers, config->worker_capacity,
           config->workers * config->worker_capacity);
    printf("\n* Transport layer by %s in %s mode\n",
           config->transport_layer_plugin->shortname,
           config->transport);
    fflush(stdout);
}

static void mk_version(void)
{
    printf("Monkey HTTP Daemon %i.%i.%i\n",
           __MONKEY__, __MONKEY_MINOR__, __MONKEY_PATCHLEVEL__);
    printf("Built : %s (%s %i.%i.%i)\n",
           MONKEY_BUILT, CC, __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
    printf("Home  : http://monkey-project.com\n");
    fflush(stdout);
}

static void mk_build_info(void)
{
    mk_version();

    printf("\n");
    printf("%s[system: %s]%s\n", ANSI_BOLD, OS, ANSI_RESET);
    printf("%s", MK_BUILD_UNAME);

    printf("\n\n%s[configure]%s\n", ANSI_BOLD, ANSI_RESET);
    printf("%s", MK_BUILD_CMD);

    printf("\n\n%s[setup]%s\n", ANSI_BOLD, ANSI_RESET);
    printf("configuration dir: %s\n", MONKEY_PATH_CONF);
    printf("\n\n");
}

static void mk_help(int rc)
{
    printf("Usage : monkey [-c directory] [-p TCP_PORT ] [-w N] [-D] [-v] [-h]\n\n");
    printf("%sAvailable options%s\n", ANSI_BOLD, ANSI_RESET);
    printf("  -c, --confdir=DIR\tspecify configuration files directory\n");
    printf("  -s, --serverconf=FILE\tspecify main server configuration file\n");
    printf("  -D, --daemon\t\trun Monkey as daemon (background mode)\n");
    printf("  -p, --port=PORT\tset listener TCP port (override config)\n");
    printf("  -w, --workers=N\tset number of workers (override config)\n\n");
    printf("%sInformational%s\n", ANSI_BOLD, ANSI_RESET);
    printf("  -b, --build\t\tprint build information\n");
    printf("  -v, --version\t\tshow version number\n");
    printf("  -h, --help\t\tprint this help\n\n");
    printf("%sDocumentation%s\n", ANSI_BOLD, ANSI_RESET);
    printf("  http://monkey-project.com/documentation\n\n");

    exit(rc);
}

/* MAIN */
int main(int argc, char **argv)
{
    int opt;
    int port_override = -1;
    int workers_override = -1;
    int run_daemon = 0;
    char *path_config = NULL;
    char *server_config = NULL;

    static const struct option long_opts[] = {
        { "configdir", required_argument, NULL, 'c' },
        { "serverconf",required_argument, NULL, 's' },
        { "build",     no_argument,       NULL, 'b' },
		{ "daemon",	   no_argument,       NULL, 'D' },
        { "port",      required_argument, NULL, 'p' },
        { "workers",   required_argument, NULL, 'w' },
        { "version",   no_argument,       NULL, 'v' },
		{ "help",	   no_argument,       NULL, 'h' },
		{ NULL, 0, NULL, 0 }
	};

    while ((opt = getopt_long(argc, argv, "bDSvhp:w:c:s:", long_opts, NULL)) != -1) {
        switch (opt) {
        case 'b':
            mk_build_info();
            exit(EXIT_SUCCESS);
        case 'v':
            mk_version();
            exit(EXIT_SUCCESS);
        case 'h':
            mk_help(EXIT_SUCCESS);
        case 'D':
            run_daemon = 1;
            break;
        case 'p':
            port_override = atoi(optarg);
            break;
        case 'w':
            workers_override = atoi(optarg);
            break;
        case 'c':
            path_config = optarg;
            break;
        case 's':
            server_config = optarg;
            break;
        case '?':
            mk_help(EXIT_FAILURE);
        }
    }

    /* setup basic configurations */
    config = mk_mem_malloc_z(sizeof(struct server_config));

    /* set configuration path */
    if (!path_config) {
        config->path_config = MONKEY_PATH_CONF;
    }
    else {
        config->path_config = path_config;
    }

    /* set target configuration file for the server */
    if (!server_config) {
        config->server_config = M_DEFAULT_CONFIG_FILE;
    }
    else {
        config->server_config = server_config;
    }

    if (run_daemon)
        config->is_daemon = MK_TRUE;
    else
        config->is_daemon = MK_FALSE;

#ifdef TRACE
    monkey_init_time = time(NULL);
    MK_TRACE("Monkey TRACE is enabled");
    env_trace_filter = getenv("MK_TRACE_FILTER");
    pthread_mutex_init(&mutex_trace, (pthread_mutexattr_t *) NULL);
#endif

    mk_version();
    mk_signal_init();

#ifdef LINUX_TRACE
    mk_info("Linux Trace enabled");
#endif

    /* Override number of thread workers */
    if (workers_override >= 0) {
        config->workers = workers_override;
    }
    else {
        config->workers = -1;
    }

    /* Core and Scheduler setup */
    mk_config_start_configure();
    mk_sched_init();

    /* Clock init that must happen before starting threads */
    mk_clock_sequential_init();

    /* Load plugins */
    mk_plugin_init();
    mk_plugin_read_config();

    /* Override TCP port if it was set in the command line */
    if (port_override > 0) {
        config->serverport = port_override;
    }

    /* Server listening socket */
    config->server_fd = mk_socket_server(config->serverport, config->listen_addr);

    /* Running Monkey as daemon */
    if (config->is_daemon == MK_TRUE) {
        mk_utils_set_daemon();
    }

    /* Register PID of Monkey */
    mk_utils_register_pid();

    /* Workers: logger and clock */
    mk_utils_worker_spawn((void *) mk_clock_worker_init, NULL);

    /* Init mk pointers */
    mk_mem_pointers_init();

    /* Init thread keys */
    mk_thread_keys_init();

    /* Change process owner */
    mk_user_set_uidgid();

    /* Configuration sanity check */
    mk_config_sanity_check();

    /* Print server details */
    mk_details();

    /* Invoke Plugin PRCTX hooks */
    mk_plugin_core_process();

    /* Launch monkey http workers */
    mk_server_launch_workers();

    /* Wait until all workers report as ready */
    while (1) {
        int i, ready = 0;

        pthread_mutex_lock(&mutex_worker_init);
        for (i = 0; i < config->workers; i++) {
            if (sched_list[i].initialized)
                ready++;
        }
        pthread_mutex_unlock(&mutex_worker_init);

        if (ready == config->workers) break;
        usleep(10000);
    }

    /* Server loop, let's listen for incomming clients */
    mk_server_loop(config->server_fd);

    mk_mem_free(config);
    return 0;
}
#endif
