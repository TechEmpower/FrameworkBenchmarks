/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2001-2012, Eduardo Silva P. <edsiper@gmail.com>
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
 *  MA 02110-1301  USA
 */

/* System headers */
#include <stdlib.h>
#include <string.h>

/* Monkey Plugin Interface */
#include "MKPlugin.h"

/* Local header files */
#include "cmd.h"
#include "cutils.h"
#include "cheetah.h"
#include "loop.h"

MONKEY_PLUGIN("cheetah",              /* shortname */
              "Cheetah! Shell",       /* name */
              VERSION,               /* version */
              MK_PLUGIN_CORE_PRCTX);  /* hooks */

void mk_cheetah_welcome_msg()
{
    CHEETAH_WRITE("\n%s%s***%s Welcome to %sCheetah!%s, the %sMonkey Shell %s:) %s***%s\n",
                  ANSI_BOLD, ANSI_YELLOW,
                  ANSI_WHITE, ANSI_GREEN,
                  ANSI_WHITE, ANSI_RED, ANSI_WHITE, ANSI_YELLOW, ANSI_RESET);
    CHEETAH_WRITE("\n      << %sType 'help' or '\\h' for help%s >>\n\n",
                  ANSI_BLUE, ANSI_RESET);
    CHEETAH_FLUSH();
}

static void mk_cheetah_config(char *path)
{
    unsigned long len;
    char *listen = NULL;
    char *default_file = NULL;
    struct mk_config *conf;
    struct mk_config_section *section;

    /* this variable is defined in cheetah.h and points to
     * the FILE *descriptor where to write out the data
     */
    cheetah_output = NULL;

    /* read configuration file */
    mk_api->str_build(&default_file, &len, "%scheetah.conf", path);
    conf = mk_api->config_create(default_file);
    section = mk_api->config_section_get(conf, "CHEETAH");

    if (!section) {
        CHEETAH_WRITE("\nError, could not find CHEETAH tag");
        exit(EXIT_FAILURE);
    }

    /* no longer needed */
    mk_api->mem_free(default_file);

    /* Listen directive */
    listen = mk_api->config_section_getval(section, "Listen",
                                           MK_CONFIG_VAL_STR);

    if (strcasecmp(listen, LISTEN_STDIN_STR) == 0) {
        listen_mode = LISTEN_STDIN;
    }
    else if (strcasecmp(listen, LISTEN_SERVER_STR) == 0) {
        listen_mode = LISTEN_SERVER;
    }
    else {
        printf("\nCheetah! Error: Invalid LISTEN value");
        exit(EXIT_FAILURE);
    }

    /* Cheetah cannot work in STDIN mode if Monkey is working in background */
    if (listen_mode == LISTEN_STDIN && mk_api->config->is_daemon == MK_TRUE) {
        printf("\nCheetah!: Forcing SERVER mode as Monkey is running in background\n");
        fflush(stdout);
        listen_mode = LISTEN_SERVER;
    }
}

static void mk_cheetah_init(void *args UNUSED_PARAM)
{
    /* Rename worker */
    mk_api->worker_rename("monkey: cheetah");

    /* Open right FDs for I/O */
    if (listen_mode == LISTEN_STDIN) {
        cheetah_input = stdin;
        cheetah_output = stdout;
        mk_cheetah_loop_stdin();
    }
    else if (listen_mode == LISTEN_SERVER) {
        mk_cheetah_loop_server();
    }
}

/* This function is called when the plugin is loaded, it must
 * return
 */
int _mkp_init(struct plugin_api **api, char *confdir)
{
    mk_api = *api;
    init_time = time(NULL);

    mk_cheetah_config(confdir);
    return 0;
}

void _mkp_exit()
{
    if (listen_mode == LISTEN_SERVER) {
        /* Remote named pipe */
        unlink(cheetah_server);
        mk_api->mem_free(cheetah_server);
    }
}

int _mkp_core_prctx(struct server_config *config)
{
    mk_api->worker_spawn(mk_cheetah_init, config);
    return 0;
}
