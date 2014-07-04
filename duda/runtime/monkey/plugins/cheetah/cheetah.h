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
 *  MA 02110-1301  USA.
 */

#ifndef MK_CHEETAH_H
#define MK_CHEETAH_H

/* Colors */
#define ANSI_BOLD "\033[1m"
#define ANSI_CYAN "\033[36m" 
#define ANSI_MAGENTA "\033[35m"
#define ANSI_RED "\033[31m"
#define ANSI_YELLOW "\033[33m"
#define ANSI_BLUE "\033[34m"
#define ANSI_GREEN "\033[32m"
#define ANSI_WHITE "\033[37m"
#define ANSI_RESET "\033[0m"

/* Commands */
#define MK_CHEETAH_CLEAR "clear"
#define MK_CHEETAH_CLEAR_SC "\\c"

#define MK_CHEETAH_CONFIG "config"
#define MK_CHEETAH_CONFIG_SC "\\f"

#define MK_CHEETAH_STATUS "status"
#define MK_CHEETAH_STATUS_SC "\\s"

#define MK_CHEETAH_HELP "help"
#define MK_CHEETAH_HELP_SC "\\h"

#define MK_CHEETAH_SHELP "?"
#define MK_CHEETAH_SHELP_SC "\\?"

#define MK_CHEETAH_UPTIME "uptime"
#define MK_CHEETAH_UPTIME_SC "\\u"

#define MK_CHEETAH_PLUGINS "plugins"
#define MK_CHEETAH_PLUGINS_SC "\\g"

#define MK_CHEETAH_VHOSTS "vhosts"
#define MK_CHEETAH_VHOSTS_SC "\\v"

#define MK_CHEETAH_WORKERS "workers"
#define MK_CHEETAH_WORKERS_SC "\\w"

#define MK_CHEETAH_QUIT "quit"
#define MK_CHEETAH_QUIT_SC "\\q"

/* Constants */
#define MK_CHEETAH_PROMPT "%s%scheetah>%s "
#define MK_CHEETAH_PROC_TASK "/proc/%i/task/%i/stat"
#define MK_CHEETAH_ONEDAY  86400
#define MK_CHEETAH_ONEHOUR  3600
#define MK_CHEETAH_ONEMINUTE  60

/* Configurarion: Listen */
#define LISTEN_STDIN_STR "STDIN"
#define LISTEN_SERVER_STR "SERVER"

#define LISTEN_STDIN 0
#define LISTEN_SERVER 1

int listen_mode;

char *cheetah_server;

int cheetah_socket;
FILE *cheetah_input;
FILE *cheetah_output;

/* functions */
void mk_cheetah_welcome_msg();

#endif
