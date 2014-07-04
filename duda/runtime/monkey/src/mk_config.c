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

/* isblank is not in C89 */
#define _GNU_SOURCE

#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>

#include "monkey.h"
#include "mk_config.h"
#include "mk_string.h"
#include "mk_utils.h"
#include "mk_mimetype.h"
#include "mk_info.h"
#include "mk_memory.h"
#include "mk_server.h"
#include "mk_plugin.h"
#include "mk_macros.h"
#include "mk_vhost.h"

struct server_config *config;

/* Raise a configuration schema error */
void mk_config_error(const char *path, int line, const char *msg)
{
    mk_err("File %s", path);
    mk_err("Error in line %i: %s", line, msg);
    exit(EXIT_FAILURE);
}

/* Returns a configuration section by [section name] */
struct mk_config_section *mk_config_section_get(struct mk_config *conf,
                                                const char *section_name)
{
    struct mk_config_section *section;
    struct mk_list *head;

    mk_list_foreach(head, &conf->sections) {
        section = mk_list_entry(head, struct mk_config_section, _head);
        if (strcasecmp(section->name, section_name) == 0) {
            return section;
        }
    }

    return NULL;
}

/* Register a new section into the configuration struct */
struct mk_config_section *mk_config_section_add(struct mk_config *conf,
                                                char *section_name)
{
    struct mk_config_section *new;

    /* Alloc section node */
    new = mk_mem_malloc(sizeof(struct mk_config_section));
    new->name = mk_string_dup(section_name);
    mk_list_init(&new->entries);
    mk_list_add(&new->_head, &conf->sections);

    return new;
}

/* Register a key/value entry in the last section available of the struct */
static void mk_config_entry_add(struct mk_config *conf,
                         const char *key, const char *val)
{
    struct mk_config_section *section;
    struct mk_config_entry *new;
    struct mk_list *head = &conf->sections;

    if (mk_list_is_empty(&conf->sections) == 0) {
        mk_err("Error: there are not sections available!");
    }

    /* Last section */
    section = mk_list_entry_last(head, struct mk_config_section, _head);

    /* Alloc new entry */
    new = mk_mem_malloc(sizeof(struct mk_config_entry));
    new->key = mk_string_dup(key);
    new->val = mk_string_dup(val);

    mk_list_add(&new->_head, &section->entries);
}

struct mk_config *mk_config_create(const char *path)
{
    int i;
    int len;
    int line = 0;
    int indent_len = -1;
    int n_keys = 0;
    char buf[255];
    char *section = NULL;
    char *indent = NULL;
    char *key, *val;
    struct mk_config *conf = NULL;
    struct mk_config_section *current = NULL;
    FILE *f;

    /* Open configuration file */
    if ((f = fopen(path, "r")) == NULL) {
        mk_warn("Config: I cannot open %s file", path);
        return NULL;
    }

    /* Alloc configuration node */
    conf = mk_mem_malloc_z(sizeof(struct mk_config));
    conf->created = time(NULL);
    conf->file = mk_string_dup(path);
    mk_list_init(&conf->sections);

    /* looking for configuration directives */
    while (fgets(buf, 255, f)) {
        len = strlen(buf);
        if (buf[len - 1] == '\n') {
            buf[--len] = 0;
            if (len && buf[len - 1] == '\r') {
                buf[--len] = 0;
            }
        }

        /* Line number */
        line++;

        if (!buf[0]) {
            continue;
        }

        /* Skip commented lines */
        if (buf[0] == '#') {
            continue;
        }

        /* Section definition */
        if (buf[0] == '[') {
            int end = -1;
            end = mk_string_char_search(buf, ']', len);
            if (end > 0) {
                /*
                 * Before to add a new section, lets check the previous
                 * one have at least one key set
                 */
                if (current && n_keys == 0) {
                    mk_config_error(path, line, "Previous section did not have keys");
                }

                /* Create new section */
                section = mk_string_copy_substr(buf, 1, end);
                current = mk_config_section_add(conf, section);
                mk_mem_free(section);
                n_keys = 0;
                continue;
            }
            else {
                mk_config_error(path, line, "Bad header definition");
            }
        }

        /* No separator defined */
        if (!indent) {
            i = 0;

            do { i++; } while (i < len && isblank(buf[i]));

            indent = mk_string_copy_substr(buf, 0, i);
            indent_len = strlen(indent);

            /* Blank indented line */
            if (i == len) {
                continue;
            }
        }


        /* Validate indentation level */
        if (strncmp(buf, indent, indent_len) != 0 ||
            isblank(buf[indent_len]) != 0) {
            mk_config_error(path, line, "Invalid indentation level");
        }

        if (buf[indent_len] == '#' || indent_len == len) {
            continue;
        }

        /* Get key and val */
        i = mk_string_char_search(buf + indent_len, ' ', len - indent_len);
        key = mk_string_copy_substr(buf + indent_len, 0, i);
        val = mk_string_copy_substr(buf + indent_len + i, 1, len - indent_len - i);

        if (!key || !val || i < 0) {
            mk_config_error(path, line, "Each key must have a value");
        }

        /* Trim strings */
        mk_string_trim(&key);
        mk_string_trim(&val);

        /* Register entry: key and val are copied as duplicated */
        mk_config_entry_add(conf, key, val);

        /* Free temporal key and val */
        mk_mem_free(key);
        mk_mem_free(val);

        n_keys++;
    }

    if (section && n_keys == 0) {
        mk_config_error(path, line, "Section do not have keys");
    }

    /*
    struct mk_config_section *s;
    struct mk_config_entry *e;

    s = conf->section;
    while(s) {
        printf("\n[%s]", s->name);
        e = s->entry;
        while(e) {
            printf("\n   %s = %s", e->key, e->val);
            e = e->next;
        }
        s = s->next;
    }
    fflush(stdout);
    */
    if (indent) mk_mem_free(indent);

    fclose(f);
    return conf;
}

void mk_config_free(struct mk_config *conf)
{
    struct mk_config_section *section;
    struct mk_list *head, *tmp;

    /* Free sections */
    mk_list_foreach_safe(head, tmp, &conf->sections) {
        section = mk_list_entry(head, struct mk_config_section, _head);
        mk_list_del(&section->_head);

        /* Free section entries */
        mk_config_free_entries(section);

        /* Free section node */
        mk_mem_free(section->name);
        mk_mem_free(section);
    }
    if (conf->file) mk_mem_free(conf->file);
    if (conf) mk_mem_free(conf);
}

void mk_config_free_entries(struct mk_config_section *section)
{
    struct mk_config_entry *entry;
    struct mk_list *head, *tmp;

    mk_list_foreach_safe(head, tmp, &section->entries) {
        entry = mk_list_entry(head, struct mk_config_entry, _head);
        mk_list_del(&entry->_head);

        /* Free memory assigned */
        mk_mem_free(entry->key);
        mk_mem_free(entry->val);
        mk_mem_free(entry);
    }
}

#ifdef SAFE_FREE
void mk_config_free_all()
{
    mk_vhost_free_all();
    if (config->config) mk_config_free(config->config);

    if (config->serverconf) mk_mem_free(config->serverconf);
    if (config->listen_addr) mk_mem_free(config->listen_addr);
    if (config->pid_file_path) mk_mem_free(config->pid_file_path);
    if (config->user_dir) mk_mem_free(config->user_dir);

    /* free config->index_files */
    if (config->index_files) {
        mk_string_split_free(config->index_files);
    }

    if (config->user) mk_mem_free(config->user);
    if (config->transport_layer) mk_mem_free(config->transport_layer);
    if (config->server_software.len) mk_pointer_free(&config->server_software);
    mk_mem_free(config);
}
#endif

void *mk_config_section_getval(struct mk_config_section *section, char *key, int mode)
{
    int on, off;
    struct mk_config_entry *entry;
    struct mk_list *head;

    mk_list_foreach(head, &section->entries) {
        entry = mk_list_entry(head, struct mk_config_entry, _head);

        if (strcasecmp(entry->key, key) == 0) {
            switch (mode) {
            case MK_CONFIG_VAL_STR:
                return (void *) mk_string_dup(entry->val);
            case MK_CONFIG_VAL_NUM:
                return (void *) strtol(entry->val, (char **) NULL, 10);
            case MK_CONFIG_VAL_BOOL:
                on = strcasecmp(entry->val, VALUE_ON);
                off = strcasecmp(entry->val, VALUE_OFF);

                if (on != 0 && off != 0) {
                    return (void *) -1;
                }
                else if (on >= 0) {
                    return (void *) MK_TRUE;
                }
                else {
                    return (void *) MK_FALSE;
                }
            case MK_CONFIG_VAL_LIST:
                return (void *)mk_string_split_line(entry->val);
            }
        }
    }
    return NULL;
}

#ifndef SHAREDLIB

/* Print a specific error */
static void mk_config_print_error_msg(char *variable, char *path)
{
    mk_err("Error in %s variable under %s, has an invalid value",
           variable, path);
    exit(EXIT_FAILURE);
}

/* Read configuration files */
static void mk_config_read_files(char *path_conf, char *file_conf)
{
    unsigned long len;
    char *tmp = NULL;
    struct stat checkdir;
    struct mk_config *cnf;
    struct mk_config_section *section;

    config->serverconf = mk_string_dup(path_conf);

    if (stat(config->serverconf, &checkdir) == -1) {
        mk_err("ERROR: Cannot find/open '%s'", config->serverconf);
        exit(EXIT_FAILURE);
    }

    mk_string_build(&tmp, &len, "%s/%s", path_conf, file_conf);

    cnf = mk_config_create(tmp);
    if (!cnf) {
        mk_err("Cannot read '%s'", config->server_config);
        exit(EXIT_FAILURE);
    }
    section = mk_config_section_get(cnf, "SERVER");

    if (!section) {
        mk_err("ERROR: No 'SERVER' section defined");
    }

    /* Map source configuration */
    config->config = cnf;

    /* Listen */
    config->listen_addr = mk_config_section_getval(section, "Listen",
                                                   MK_CONFIG_VAL_STR);
    if (!config->listen_addr) {
        config->listen_addr = mk_string_dup(MK_DEFAULT_LISTEN_ADDR);
    }

    /* Connection port */
    config->serverport = (size_t) mk_config_section_getval(section,
                                                           "Port",
                                                           MK_CONFIG_VAL_NUM);
    if (config->serverport <= 1 || config->serverport >= 65535) {
        mk_config_print_error_msg("Port", tmp);
    }

    /* Number of thread workers */
    if (config->workers == -1) {
        config->workers = (size_t) mk_config_section_getval(section,
                                                            "Workers",
                                                            MK_CONFIG_VAL_NUM);
    }

    if (config->workers < 1) {
        config->workers = sysconf(_SC_NPROCESSORS_ONLN);
        if (config->workers < 1) {
            mk_config_print_error_msg("Workers", tmp);
        }
    }

    /* Get each worker clients capacity based on FDs system limits */
    config->worker_capacity = mk_server_worker_capacity(config->workers);

    /* Set max server load */
    config->max_load = (config->worker_capacity * config->workers);

    /* Timeout */
    config->timeout = (size_t) mk_config_section_getval(section,
                                                     "Timeout", MK_CONFIG_VAL_NUM);
    if (config->timeout < 1) {
        mk_config_print_error_msg("Timeout", tmp);
    }

    /* KeepAlive */
    config->keep_alive = (size_t) mk_config_section_getval(section,
                                                        "KeepAlive",
                                                        MK_CONFIG_VAL_BOOL);
    if (config->keep_alive == MK_ERROR) {
        mk_config_print_error_msg("KeepAlive", tmp);
    }

    /* MaxKeepAliveRequest */
    config->max_keep_alive_request = (size_t)
        mk_config_section_getval(section,
                                 "MaxKeepAliveRequest",
                                 MK_CONFIG_VAL_NUM);

    if (config->max_keep_alive_request == 0) {
        mk_config_print_error_msg("MaxKeepAliveRequest", tmp);
    }

    /* KeepAliveTimeout */
    config->keep_alive_timeout = (size_t) mk_config_section_getval(section,
                                                                "KeepAliveTimeout",
                                                                MK_CONFIG_VAL_NUM);
    if (config->keep_alive_timeout == 0) {
        mk_config_print_error_msg("KeepAliveTimeout", tmp);
    }

    /* Pid File */
    config->pid_file_path = mk_config_section_getval(section,
                                                     "PidFile", MK_CONFIG_VAL_STR);

    /* Home user's directory /~ */
    config->user_dir = mk_config_section_getval(section,
                                                "UserDir", MK_CONFIG_VAL_STR);

    /* Index files */
    config->index_files = mk_config_section_getval(section,
                                                   "Indexfile", MK_CONFIG_VAL_LIST);

    /* HideVersion Variable */
    config->hideversion = (size_t) mk_config_section_getval(section,
                                                         "HideVersion",
                                                         MK_CONFIG_VAL_BOOL);
    if (config->hideversion == MK_ERROR) {
        mk_config_print_error_msg("HideVersion", tmp);
    }

    /* User Variable */
    config->user = mk_config_section_getval(section, "User", MK_CONFIG_VAL_STR);

    /* Resume */
    config->resume = (size_t) mk_config_section_getval(section,
                                                    "Resume", MK_CONFIG_VAL_BOOL);
    if (config->resume == MK_ERROR) {
        mk_config_print_error_msg("Resume", tmp);
    }

    /* Max Request Size */
    config->max_request_size = (size_t) mk_config_section_getval(section,
                                                              "MaxRequestSize",
                                                              MK_CONFIG_VAL_NUM);
    if (config->max_request_size <= 0) {
        mk_config_print_error_msg("MaxRequestSize", tmp);
    }
    else {
        config->max_request_size *= 1024;
    }

    /* Symbolic Links */
    config->symlink = (size_t) mk_config_section_getval(section,
                                                     "SymLink", MK_CONFIG_VAL_BOOL);
    if (config->symlink == MK_ERROR) {
        mk_config_print_error_msg("SymLink", tmp);
    }

    /* Transport Layer plugin */
    config->transport_layer = mk_config_section_getval(section,
                                                       "TransportLayer",
                                                       MK_CONFIG_VAL_STR);

    /* Default Mimetype */
    tmp = mk_config_section_getval(section, "DefaultMimeType", MK_CONFIG_VAL_STR);
    if (!tmp) {
        config->default_mimetype = mk_string_dup(MIMETYPE_DEFAULT_TYPE);
    }
    else {
        mk_string_build(&config->default_mimetype, &len, "%s\r\n", tmp);
        mk_mem_free(tmp);
    }

    /* File Descriptor Table (FDT) */
    config->fdt = (size_t) mk_config_section_getval(section,
                                                    "FDT",
                                                    MK_CONFIG_VAL_BOOL);
    mk_vhost_init(path_conf);
}

/* read main configuration from monkey.conf */
void mk_config_start_configure(void)
{
    unsigned long len;

    mk_config_set_init_values();
    mk_config_read_files(config->path_config, config->server_config);

    /* Load mimes */
    mk_mimetype_read_config();

    mk_pointer_reset(&config->server_software);

    /* Basic server information */
    if (config->hideversion == MK_FALSE) {
        mk_string_build(&config->server_software.data,
                        &len, "Monkey/%s (%s)", VERSION, OS);
        config->server_software.len = len;
    }
    else {
        mk_string_build(&config->server_software.data, &len, "Monkey Server");
        config->server_software.len = len;
    }
}

#endif // !SHAREDLIB

void mk_config_set_init_values(void)
{
    /* Init values */
    config->is_seteuid = MK_FALSE;
    config->timeout = 15;
    config->hideversion = MK_FALSE;
    config->keep_alive = MK_TRUE;
    config->keep_alive_timeout = 15;
    config->max_keep_alive_request = 50;
    config->resume = MK_TRUE;
    config->standard_port = 80;
    config->listen_addr = MK_DEFAULT_LISTEN_ADDR;
    config->serverport = 2001;
    config->symlink = MK_FALSE;
    config->nhosts = 0;
    mk_list_init(&config->hosts);
    config->user = NULL;
    config->open_flags = O_RDONLY | O_NONBLOCK;
    config->index_files = NULL;
    config->user_dir = NULL;

    /* Max request buffer size allowed
     * right now, every chunk size is 4KB (4096 bytes),
     * so we are setting a maximum request size to 32 KB */
    config->max_request_size = MK_REQUEST_CHUNK * 8;

    /* Plugins */
    config->plugins = mk_mem_malloc(sizeof(struct mk_list));

    /* Internals */
    config->safe_event_write = MK_FALSE;

    /*
     * Transport type: useful to build redirection headers, values:
     *
     *   MK_TRANSPORT_HTTP
     *   MK_TRANSPORT_HTTPS
     *
     * we set default to 'http'
     */
    config->transport = MK_TRANSPORT_HTTP;
    config->transport_layer = NULL;

    /* Init plugin list */
    mk_list_init(config->plugins);
}


void mk_config_sanity_check()
{
    /* Check O_NOATIME for current user, flag will just be used
     * if running user is allowed to.
     */
    int fd, flags = config->open_flags;

    flags |= O_NOATIME;
    fd = open(config->path_config, flags);

    if (fd > -1) {
        config->open_flags = flags;
        close(fd);
    }
}
