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

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <ctype.h>

#include "MKPlugin.h"
#include "cheetah.h"
#include "cutils.h"
#include "cmd.h"

/* strip leading and trailing space from input command line. */
static char *strip_whitespace(char *cmd)
{
    char *end;
    while (isspace(*cmd))
        cmd++;
    if (*cmd == 0)
        return cmd;
    end = cmd + strlen(cmd) - 1;
    while (end > cmd && isspace(*end))
        end--;
    end++;
    *end = '\0';
    return cmd;
}

int mk_cheetah_cmd(char *raw_cmd)
{
    char *cmd = strip_whitespace(raw_cmd);
    if (strcmp(cmd, MK_CHEETAH_CONFIG) == 0 ||
        strcmp(cmd, MK_CHEETAH_CONFIG_SC) == 0) {
        mk_cheetah_cmd_config();
    }
    else if (strcmp(cmd, MK_CHEETAH_STATUS) == 0 ||
        strcmp(cmd, MK_CHEETAH_STATUS_SC) == 0) {
        mk_cheetah_cmd_status();
    }
    else if (strcmp(cmd, MK_CHEETAH_CLEAR) == 0 ||
             strcmp(cmd, MK_CHEETAH_CLEAR_SC) == 0) {
        mk_cheetah_cmd_clear();
    }
    else if (strcmp(cmd, MK_CHEETAH_UPTIME) == 0 ||
             strcmp(cmd, MK_CHEETAH_UPTIME_SC) == 0) {
        mk_cheetah_cmd_uptime();
    }
    else if (strcmp(cmd, MK_CHEETAH_PLUGINS) == 0 ||
             strcmp(cmd, MK_CHEETAH_PLUGINS_SC) == 0) {
        mk_cheetah_cmd_plugins();
    }
    else if (strcmp(cmd, MK_CHEETAH_WORKERS) == 0 ||
             strcmp(cmd, MK_CHEETAH_WORKERS_SC) == 0) {
        mk_cheetah_cmd_workers();
    }
    else if (strcmp(cmd, MK_CHEETAH_VHOSTS) == 0 ||
             strcmp(cmd, MK_CHEETAH_VHOSTS_SC) == 0) {
        mk_cheetah_cmd_vhosts();
    }
    else if (strcmp(cmd, MK_CHEETAH_HELP) == 0 ||
             strcmp(cmd, MK_CHEETAH_HELP_SC) == 0 ||
             strcmp(cmd, MK_CHEETAH_SHELP) == 0 ||
             strcmp(cmd, MK_CHEETAH_SHELP_SC) == 0) {
        mk_cheetah_cmd_help();
    }
    else if (strcmp(cmd, MK_CHEETAH_QUIT) == 0 ||
             strcmp(cmd, MK_CHEETAH_QUIT_SC) == 0) {
        return mk_cheetah_cmd_quit();
    }
    else if (strlen(cmd) == 0) {
        return 0;
    }
    else {
      CHEETAH_WRITE("Invalid command, type 'help' for a list of available commands\n");
    }

    CHEETAH_FLUSH();
    return 0;
}

void mk_cheetah_cmd_clear()
{
    CHEETAH_WRITE("\033[2J\033[1;1H");
}

void mk_cheetah_cmd_uptime()
{
    int days;
    int hours;
    int minutes;
    int seconds;
    long int upmind;
    long int upminh;
    long int uptime;

    /* uptime in seconds */
    uptime = time(NULL) - init_time;

    /* days */
    days = uptime / MK_CHEETAH_ONEDAY;
    upmind = uptime - (days * MK_CHEETAH_ONEDAY);

    /* hours */
    hours = upmind / MK_CHEETAH_ONEHOUR;
    upminh = upmind - hours * MK_CHEETAH_ONEHOUR;

    /* minutes */
    minutes = upminh / MK_CHEETAH_ONEMINUTE;
    seconds = upminh - minutes * MK_CHEETAH_ONEMINUTE;

    CHEETAH_WRITE
        ("Server has been running: %i day%s, %i hour%s, %i minute%s and %i second%s\n\n",
         days, (days > 1) ? "s" : "", hours, (hours > 1) ? "s" : "", minutes,
         (minutes > 1) ? "s" : "", seconds, (seconds > 1) ? "s" : "");
}

void mk_cheetah_cmd_plugins_print_stage(struct mk_list *list, const char *stage,
                                        int stage_bw)
{
    struct plugin *p;
    struct mk_list *head;

    if (mk_list_is_empty(list) == 0) {
        return;
    }

    CHEETAH_WRITE("%s[%s]%s", ANSI_BOLD ANSI_YELLOW, stage, ANSI_RESET);

    mk_list_foreach(head, list) {
        p = mk_list_entry(head, struct plugin, _head);
        if (p->hooks & stage_bw) {
            CHEETAH_WRITE("\n  [%s] %s v%s on \"%s\"",
                          p->shortname, p->name, p->version, p->path);
        }
    }

    CHEETAH_WRITE("\n\n");
}

void mk_cheetah_cmd_plugins_print_core(struct mk_list *list)
{
    struct plugin *p;
    struct mk_list *head;

    CHEETAH_WRITE("\n%s[CORE PROCESS CONTEXT]%s", ANSI_BOLD ANSI_BLUE, ANSI_RESET);

    mk_list_foreach(head, list) {
        p = mk_list_entry(head, struct plugin, _head);

        if (p->hooks & MK_PLUGIN_CORE_PRCTX) {
            CHEETAH_WRITE("\n  [%s] %s v%s on \"%s\"",
                          p->shortname, p->name, p->version, p->path);
        }
    }

    CHEETAH_WRITE("\n\n%s[CORE THREAD CONTEXT]%s", ANSI_BOLD ANSI_BLUE, ANSI_RESET);

    mk_list_foreach(head, list) {
        p = mk_list_entry(head, struct plugin, _head);

        if (p->hooks & MK_PLUGIN_CORE_THCTX) {
            CHEETAH_WRITE("\n  [%s] %s v%s on \"%s\"",
                   p->shortname, p->name, p->version, p->path);
        }
    }

    CHEETAH_WRITE("\n\n");
}

void mk_cheetah_cmd_plugins_print_network(struct mk_list *list)
{
    struct plugin *p;
    struct mk_list *head;

    CHEETAH_WRITE("%s[NETWORK I/O]%s", ANSI_BOLD ANSI_RED, ANSI_RESET);

    mk_list_foreach(head, list) {
        p = mk_list_entry(head, struct plugin, _head);
        if (p->hooks & MK_PLUGIN_NETWORK_IO) {
            CHEETAH_WRITE("\n  [%s] %s v%s on \"%s\"",
                          p->shortname, p->name, p->version, p->path);
        }
    }

    CHEETAH_WRITE("\n");
}

void mk_cheetah_cmd_plugins()
{
    struct mk_list *list = mk_api->plugins;

    CHEETAH_WRITE("List of plugins and hooks associated\n");

    if (!list) {
        return;
    }

    mk_cheetah_cmd_plugins_print_core(list);
    mk_cheetah_cmd_plugins_print_stage(list, "STAGE_10", MK_PLUGIN_STAGE_10);
    mk_cheetah_cmd_plugins_print_stage(list, "STAGE_20", MK_PLUGIN_STAGE_20);
    mk_cheetah_cmd_plugins_print_stage(list, "STAGE_30", MK_PLUGIN_STAGE_30);
    mk_cheetah_cmd_plugins_print_stage(list, "STAGE_40", MK_PLUGIN_STAGE_40);
    mk_cheetah_cmd_plugins_print_stage(list, "STAGE_50", MK_PLUGIN_STAGE_50);
    mk_cheetah_cmd_plugins_print_network(list);

    CHEETAH_WRITE("\n");
}

void mk_cheetah_cmd_vhosts()
{
    struct host *entry_host;
    struct host_alias *entry_alias;
    struct mk_config_section *section;
    struct mk_config_entry *entry;
    struct mk_list *hosts = &mk_api->config->hosts;
    struct mk_list *aliases;
    struct mk_list *head_host;
    struct mk_list *head_alias;
    struct mk_list *head_sections;
    struct mk_list *head_entries;

    mk_list_foreach(head_host, hosts) {
        entry_host = mk_list_entry(head_host, struct host, _head);

        aliases = &entry_host->server_names;
        entry_alias = mk_list_entry_first(aliases, struct host_alias, _head);
        CHEETAH_WRITE("%s[%sVHost '%s'%s%s]%s\n",
                      ANSI_BOLD, ANSI_YELLOW,
                      entry_alias->name, ANSI_BOLD, ANSI_WHITE, ANSI_RESET);

        CHEETAH_WRITE("      - Names         : ");
        mk_list_foreach(head_alias, aliases) {
            entry_alias = mk_list_entry(head_alias, struct host_alias, _head);
            CHEETAH_WRITE("%s ", entry_alias->name);
        }
        CHEETAH_WRITE("\n");

        CHEETAH_WRITE("      - Document root : %s\n", entry_host->documentroot.data);
        CHEETAH_WRITE("      - Config file   : %s\n", entry_host->file);

        if (!entry_host->config) {
            continue;
        }

        mk_list_foreach(head_sections, &entry_host->config->sections) {
            section = mk_list_entry(head_sections, struct mk_config_section, _head);
            CHEETAH_WRITE("      %s+%s [%s]\n", ANSI_GREEN, ANSI_RESET,
                          section->name);

            mk_list_foreach(head_entries, &section->entries) {
                entry = mk_list_entry(head_entries, struct mk_config_entry, _head);
                    CHEETAH_WRITE("        - %11.10s : %s\n", entry->key, entry->val);
            }
        }
    }

    CHEETAH_WRITE("\n");
}

void mk_cheetah_cmd_workers()
{
    int i;
    unsigned long long active_connections;
    struct sched_list_node *node;

    node = mk_api->sched_list;
    for (i=0; i < mk_api->config->workers; i++) {
        active_connections = (node[i].accepted_connections - node[i].closed_connections);

        CHEETAH_WRITE("* Worker %i\n", node[i].idx);
        CHEETAH_WRITE("      - Task ID           : %i\n", node[i].pid);
        CHEETAH_WRITE("      - Active Connections: %llu\n", active_connections);
    }

    CHEETAH_WRITE("\n");
}

int mk_cheetah_cmd_quit()
{
    CHEETAH_WRITE("Cheeta says: Good Bye!\n");
    if (listen_mode == LISTEN_STDIN) {
        pthread_exit(NULL);
        return 0;
    }
    else {
        return -1;
    }
}

void mk_cheetah_cmd_help()
{
    CHEETAH_WRITE("List of available commands for Cheetah Shell\n");
    CHEETAH_WRITE("\ncommand  shortcut  description");
    CHEETAH_WRITE("\n----------------------------------------------------");
    CHEETAH_WRITE("\n?          (\\?)    Synonym for 'help'");
    CHEETAH_WRITE("\nconfig     (\\f)    Display global configuration");
    CHEETAH_WRITE("\nplugins    (\\g)    List loaded plugins and associated stages");
    CHEETAH_WRITE("\nstatus     (\\s)    Display general web server information");
    CHEETAH_WRITE("\nuptime     (\\u)    Display how long the web server has been running");
    CHEETAH_WRITE("\nvhosts     (\\v)    List virtual hosts configured");
    CHEETAH_WRITE("\nworkers    (\\w)    Show thread workers information\n");
    CHEETAH_WRITE("\nclear      (\\c)    Clear screen");
    CHEETAH_WRITE("\nhelp       (\\h)    Print this help");
    CHEETAH_WRITE("\nquit       (\\q)    Exit Cheetah shell :_(\n\n");
}

void mk_cheetah_cmd_config()
{
    struct mk_string_line *entry;
    struct mk_list *head;

    CHEETAH_WRITE("Basic configuration");
    CHEETAH_WRITE("\n-------------------");
    CHEETAH_WRITE("\nServer Port     : %i", mk_api->config->serverport);

    if (strcmp(mk_api->config->listen_addr, "0.0.0.0") == 0) {
        CHEETAH_WRITE("\nListen          : All interfaces");
    }
    else {
        CHEETAH_WRITE("\nListen          : %s", mk_api->config->listen_addr);
    }
    CHEETAH_WRITE("\nWorkers         : %i threads", mk_api->config->workers);
    CHEETAH_WRITE("\nTimeout         : %i seconds", mk_api->config->timeout);
    CHEETAH_WRITE("\nPidFile         : %s.%i",
                  mk_api->config->pid_file_path,
                  mk_api->config->serverport);
    CHEETAH_WRITE("\nUserDir         : %s", mk_api->config->user_dir);


    if (mk_list_is_empty(mk_api->config->index_files) == 0) {
        CHEETAH_WRITE("\nIndexFile       : No index files defined");
    }
    else {
        CHEETAH_WRITE("\nIndexFile       : ");
        mk_list_foreach(head, mk_api->config->index_files) {
            entry = mk_list_entry(head, struct mk_string_line, _head);
            CHEETAH_WRITE("%s ", entry->val);
        }

    }

    CHEETAH_WRITE("\nHideVersion     : ");
    if (mk_api->config->hideversion == MK_TRUE) {
        CHEETAH_WRITE("On");
    }
    else {
        CHEETAH_WRITE("Off");
    }

    CHEETAH_WRITE("\nResume          : ");
    if (mk_api->config->resume == MK_TRUE) {
        CHEETAH_WRITE("On");
    }
    else {
        CHEETAH_WRITE("Off");
    }

    CHEETAH_WRITE("\nUser            : %s", mk_api->config->user);
    CHEETAH_WRITE("\n\nAdvanced configuration");
    CHEETAH_WRITE("\n----------------------");
    CHEETAH_WRITE("\nKeepAlive           : ");
    if (mk_api->config->keep_alive == MK_TRUE) {
        CHEETAH_WRITE("On");
    }
    else {
        CHEETAH_WRITE("Off");
    }
    CHEETAH_WRITE("\nMaxKeepAliveRequest : %i req/connection",
           mk_api->config->max_keep_alive_request);
    CHEETAH_WRITE("\nKeepAliveTimeout    : %i seconds", mk_api->config->keep_alive_timeout);
    CHEETAH_WRITE("\nMaxRequestSize      : %i KB",
           mk_api->config->max_request_size/1024);
    CHEETAH_WRITE("\nSymLink             : ");
    if (mk_api->config->symlink == MK_TRUE) {
        CHEETAH_WRITE("On");
    }
    else {
        CHEETAH_WRITE("Off");
    }
    CHEETAH_WRITE("\n\n");
}

void mk_cheetah_cmd_status()
{
    int nthreads = mk_api->config->workers;

    /* FIXME */
    //CHEETAH_WRITE("Cheetah Plugin v%s\n\n", _plugin_info->version);
    CHEETAH_WRITE("Monkey Version     : %s\n", VERSION);
    CHEETAH_WRITE("Configutarion path : %s\n", mk_api->config->serverconf);

    CHEETAH_WRITE("Cheetah! mode      : ");
    if (listen_mode == LISTEN_STDIN) {
        CHEETAH_WRITE("STDIN\n");
    }
    else {
        CHEETAH_WRITE("SERVER @ %s\n", cheetah_server);
    }

    CHEETAH_WRITE("Process ID         : %i\n", getpid());
    CHEETAH_WRITE("Process User       : ");
    mk_cheetah_print_running_user();

    CHEETAH_WRITE("Server Port        : %i\n", mk_api->config->serverport);
    CHEETAH_WRITE("Worker Threads     : %i (per configuration: %i)\n\n",
           nthreads, mk_api->config->workers);

}


