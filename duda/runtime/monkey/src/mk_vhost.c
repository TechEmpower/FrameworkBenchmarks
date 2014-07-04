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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>


#include "mk_list.h"
#include "mk_vhost.h"
#include "mk_utils.h"
#include "mk_macros.h"
#include "mk_config.h"
#include "mk_string.h"
#include "mk_http_status.h"
#include "mk_memory.h"
#include "mk_request.h"
#include "mk_info.h"

/* Initialize Virtual Host FDT mutex */
pthread_mutex_t mk_vhost_fdt_mutex = PTHREAD_MUTEX_INITIALIZER;

static __thread struct mk_list *mk_vhost_fdt_key;

/*
 * This function is triggered upon thread creation (inside the thread
 * context), here we configure per-thread data.
 */
int mk_vhost_fdt_worker_init()
{
    int i;
    int j;
    struct host *h;
    struct mk_list *list;
    struct mk_list *head;
    struct vhost_fdt_host *fdt;
    struct vhost_fdt_hash_table *ht;
    struct vhost_fdt_hash_chain *hc;

    if (config->fdt == MK_FALSE) {
        return -1;
    }

    /*
     * We are under a thread context and the main configuration is
     * already in place. Now for every existent virtual host we are
     * going to create the File Descriptor Table (FDT) which aims to
     * hold references of 'open and shared' file descriptors under
     * the Virtual Host context.
     */

    /*
     * Under an initialization context we need to protect this critical
     * section
     */
    pthread_mutex_lock(&mk_vhost_fdt_mutex);

    /*
     * Initialize the thread FDT/Hosts list and create an entry per
     * existent virtual host
     */
    list = mk_mem_malloc_z(sizeof(struct mk_list));
    mk_list_init(list);

    mk_list_foreach(head, &config->hosts) {
        h = mk_list_entry(head, struct host, _head);

        fdt = mk_mem_malloc(sizeof(struct vhost_fdt_host));
        fdt->host = h;

        /* Initialize hash table */
        for (i = 0; i < VHOST_FDT_HASHTABLE_SIZE; i++) {
            ht = &fdt->hash_table[i];
            ht->av_slots = VHOST_FDT_HASHTABLE_CHAINS;

            /* for each chain under the hash table, set the fd */
            for (j = 0; j < VHOST_FDT_HASHTABLE_CHAINS; j++) {
                hc = &ht->chain[j];
                hc->fd      = -1;
                hc->hash    =  0;
                hc->readers =  0;
            }
        }
        mk_list_add(&fdt->_head, list);
    }

    mk_vhost_fdt_key = list;
    pthread_mutex_unlock(&mk_vhost_fdt_mutex);

    return 0;
}

static inline
struct vhost_fdt_hash_table *mk_vhost_fdt_table_lookup(int id, struct host *host)
{
    struct mk_list *head;
    struct mk_list *vhost_list;
    struct vhost_fdt_host *fdt_host;
    struct vhost_fdt_hash_table *ht = NULL;

    vhost_list = mk_vhost_fdt_key;
    mk_list_foreach(head, vhost_list) {
        fdt_host = mk_list_entry(head, struct vhost_fdt_host, _head);
        if (fdt_host->host == host) {
            ht = &fdt_host->hash_table[id];
            return ht;
        }
    }

    return ht;
}

static inline
struct vhost_fdt_hash_chain
*mk_vhost_fdt_chain_lookup(unsigned int hash, struct vhost_fdt_hash_table *ht)
{
    int i;
    struct vhost_fdt_hash_chain *hc = NULL;

    for (i = 0; i < VHOST_FDT_HASHTABLE_CHAINS; i++) {
        hc = &ht->chain[i];
        if (hc->hash == hash) {
            return hc;
        }
    }

    return NULL;
}


static inline int mk_vhost_fdt_open(int id, unsigned int hash,
                                    struct session_request *sr)
{
    int i;
    int fd;
    struct vhost_fdt_hash_table *ht = NULL;
    struct vhost_fdt_hash_chain *hc;

    if (config->fdt == MK_FALSE) {
        return open(sr->real_path.data, sr->file_info.flags_read_only);
    }

    ht = mk_vhost_fdt_table_lookup(id, sr->host_conf);
    if (mk_unlikely(!ht)) {
        return open(sr->real_path.data, sr->file_info.flags_read_only);
    }

    /* We got the hash table, now look around the chains array */
    hc = mk_vhost_fdt_chain_lookup(hash, ht);
    if (hc) {
        /* Increment the readers and return the shared FD */
        hc->readers++;
        return hc->fd;
    }

    /*
     * Get here means that no entry exists in the hash table for the
     * requested file descriptor and hash, we must try to open the file
     * and register the entry in the table.
     */
    fd = open(sr->real_path.data, sr->file_info.flags_read_only);
    if (fd == -1) {
        return -1;
    }

    /* If chains are full, just return the new FD, bad luck... */
    if (ht->av_slots <= 0) {
        return fd;
    }

    /* Register the new entry in an available slot */
    for (i = 0; i < VHOST_FDT_HASHTABLE_CHAINS; i++) {
        hc = &ht->chain[i];
        if (hc->fd == -1) {
            hc->fd   = fd;
            hc->hash = hash;
            hc->readers++;
            ht->av_slots--;

            sr->vhost_fdt_id   = id;
            sr->vhost_fdt_hash = hash;

            return fd;
        }
    }

    return -1;
}

static inline int mk_vhost_fdt_close(struct session_request *sr)
{
    int id;
    unsigned int hash;
    struct vhost_fdt_hash_table *ht = NULL;
    struct vhost_fdt_hash_chain *hc;

    if (config->fdt == MK_FALSE) {
        return close(sr->fd_file);
    }

    id   = sr->vhost_fdt_id;
    hash = sr->vhost_fdt_hash;

    ht = mk_vhost_fdt_table_lookup(id, sr->host_conf);
    if (mk_unlikely(!ht)) {
        return close(sr->fd_file);
    }

    /* We got the hash table, now look around the chains array */
    hc = mk_vhost_fdt_chain_lookup(hash, ht);
    if (hc) {
        /* Increment the readers and check if we should close */
        hc->readers--;
        if (hc->readers == 0) {
            hc->fd   = -1;
            hc->hash = 0;
            ht->av_slots++;
            return close(sr->fd_file);
        }
        else {
            return 0;
        }
    }

    return close(sr->fd_file);
}


int mk_vhost_open(struct session_request *sr)
{
    int id;
    int off;
    unsigned int hash;

    //return open(sr->real_path.data, sr->file_info.flags_read_only);

    off = sr->host_conf->documentroot.len;
    hash = mk_utils_gen_hash(sr->real_path.data + off,
                             sr->real_path.len - off);
    id   = (hash % VHOST_FDT_HASHTABLE_SIZE);

    return mk_vhost_fdt_open(id, hash, sr);
}

int mk_vhost_close(struct session_request *sr)
{
    //return close(sr->fd_file);

    return mk_vhost_fdt_close(sr);
}

/*
 * Open a virtual host configuration file and return a structure with
 * definitions.
 */
struct host *mk_vhost_read(char *path)
{
    unsigned long len = 0;
    char *host_low;
    struct stat checkdir;
    struct host *host;
    struct host_alias *new_alias;
    struct error_page *err_page;
    struct mk_config *cnf;
    struct mk_config_section *section_host;
    struct mk_config_section *section_ep;
    struct mk_config_entry *entry_ep;
    struct mk_string_line *entry;
    struct mk_list *head, *list;

    /* Read configuration file */
    cnf = mk_config_create(path);
    if (!cnf) {
        mk_err("Configuration error, aborting.");
        exit(EXIT_FAILURE);
    }

    /* Read tag 'HOST' */
    section_host = mk_config_section_get(cnf, "HOST");
    if (!section_host) {
        mk_err("Invalid config file %s", path);
        return NULL;
    }

    /* Alloc configuration node */
    host = mk_mem_malloc_z(sizeof(struct host));
    host->config = cnf;
    host->file = mk_string_dup(path);

    /* Init list for custom error pages */
    mk_list_init(&host->error_pages);

    /* Init list for host name aliases */
    mk_list_init(&host->server_names);

    /* Lookup Servername */
    list = mk_config_section_getval(section_host, "Servername", MK_CONFIG_VAL_LIST);
    if (!list) {
        mk_err("Hostname does not contain a Servername");
        exit(EXIT_FAILURE);
    }

    mk_list_foreach(head, list) {
        entry = mk_list_entry(head, struct mk_string_line, _head);
        if (entry->len > MK_HOSTNAME_LEN - 1) {
            continue;
        }

        /* Hostname to lowercase */
        host_low = mk_string_tolower(entry->val);

        /* Alloc node */
        new_alias = mk_mem_malloc_z(sizeof(struct host_alias));
        new_alias->name = mk_mem_malloc_z(entry->len + 1);
        strncpy(new_alias->name, host_low, entry->len);
        mk_mem_free(host_low);

        new_alias->len = entry->len;

        mk_list_add(&new_alias->_head, &host->server_names);
    }
    mk_string_split_free(list);

    /* Lookup document root handled by a mk_pointer */
    host->documentroot.data = mk_config_section_getval(section_host,
                                                       "DocumentRoot",
                                                       MK_CONFIG_VAL_STR);
    host->documentroot.len = strlen(host->documentroot.data);

    /* Validate document root configured */
    if (stat(host->documentroot.data, &checkdir) == -1) {
        mk_err("Invalid path to DocumentRoot in %s", path);
    }
    else if (!(checkdir.st_mode & S_IFDIR)) {
        mk_err("DocumentRoot variable in %s has an invalid directory path", path);
    }

    if (mk_list_is_empty(&host->server_names) == 0) {
        mk_config_free(cnf);
        return NULL;
    }

    /* Error Pages */
    section_ep = mk_config_section_get(cnf, "ERROR_PAGES");
    if (section_ep) {
        mk_list_foreach(head, &section_ep->entries) {
            entry_ep = mk_list_entry(head, struct mk_config_entry, _head);

            int ep_status = -1;
            char *ep_file = NULL;
            unsigned long len;

            ep_status = atoi(entry_ep->key);
            ep_file   = entry_ep->val;

            /* Validate input values */
            if (ep_status < MK_CLIENT_BAD_REQUEST ||
                ep_status > MK_SERVER_HTTP_VERSION_UNSUP ||
                ep_file == NULL) {
                continue;
            }

            /* Alloc error page node */
            err_page = mk_mem_malloc_z(sizeof(struct error_page));
            err_page->status = ep_status;
            err_page->file   = mk_string_dup(ep_file);
            err_page->real_path = NULL;
            mk_string_build(&err_page->real_path, &len, "%s/%s",
                            host->documentroot.data, err_page->file);

            MK_TRACE("Map error page: status %i -> %s", err_page->status, err_page->file);

            /* Link page to the error page list */
            mk_list_add(&err_page->_head, &host->error_pages);
        }
    }

    /* Server Signature */
    if (config->hideversion == MK_FALSE) {
        mk_string_build(&host->host_signature, &len,
                        "Monkey/%s", VERSION);
    }
    else {
        mk_string_build(&host->host_signature, &len, "Monkey");
    }
    mk_string_build(&host->header_host_signature.data,
                    &host->header_host_signature.len,
                    "Server: %s", host->host_signature);

    return host;
}

/* Given a configuration directory, start reading the virtual host entries */
void mk_vhost_init(char *path)
{
    DIR *dir;
    unsigned long len;
    char *buf = 0;
    char *file;
    struct host *p_host;     /* debug */
    struct dirent *ent;

    /* Read default virtual host file */
    mk_string_build(&buf, &len, "%s/sites/default", path);
    p_host = mk_vhost_read(buf);
    if (!p_host) {
        mk_err("Error parsing main configuration file 'default'");
    }
    mk_list_add(&p_host->_head, &config->hosts);
    config->nhosts++;
    mk_mem_free(buf);
    buf = NULL;


    /* Read all virtual hosts defined in sites/ */
    mk_string_build(&buf, &len, "%s/sites/", path);
    if (!(dir = opendir(buf))) {
        mk_err("Could not open %s", buf);
        mk_mem_free(buf);
        exit(EXIT_FAILURE);
    }
    mk_mem_free(buf);

    /* Reading content */
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp((char *) ent->d_name, ".") == 0)
            continue;
        if (strcmp((char *) ent->d_name, "..") == 0)
            continue;
        if (strcasecmp((char *) ent->d_name, "default") == 0)
            continue;

        file = NULL;
        mk_string_build(&file, &len, "%s/sites/%s", path, ent->d_name);

        p_host = mk_vhost_read(file);
        mk_mem_free(file);
        if (!p_host) {
            continue;
        }
        else {
            mk_list_add(&p_host->_head, &config->hosts);
            config->nhosts++;
        }
    }
    closedir(dir);
}


/* Lookup a registered virtual host based on the given 'host' input */
int mk_vhost_get(mk_pointer host, struct host **vhost, struct host_alias **alias)
{
    struct host *entry_host;
    struct host_alias *entry_alias;
    struct mk_list *head_vhost, *head_alias;

    mk_list_foreach(head_vhost, &config->hosts) {
        entry_host = mk_list_entry(head_vhost, struct host, _head);
        mk_list_foreach(head_alias, &entry_host->server_names) {
            entry_alias = mk_list_entry(head_alias, struct host_alias, _head);
            if (entry_alias->len == host.len &&
                strncmp(entry_alias->name, host.data, host.len) == 0) {
                *vhost = entry_host;
                *alias = entry_alias;
                return 0;
            }
        }
    }

    return -1;
}

#ifdef SAFE_FREE
void mk_vhost_free_all()
{
    struct host *host;
    struct host_alias *host_alias;
    struct mk_list *head_host;
    struct mk_list *head_alias;
    struct mk_list *tmp1, *tmp2;

    mk_list_foreach_safe(head_host, tmp1, &config->hosts) {
        host = mk_list_entry(head_host, struct host, _head);
        mk_list_del(&host->_head);

        mk_mem_free(host->file);

        /* Free aliases or servernames */
        mk_list_foreach_safe(head_alias, tmp2, &host->server_names) {
            host_alias = mk_list_entry(head_alias, struct host_alias, _head);
            mk_list_del(&host_alias->_head);
            mk_mem_free(host_alias->name);
            mk_mem_free(host_alias);
        }

        mk_pointer_free(&host->documentroot);
        mk_mem_free(host->host_signature);
        mk_pointer_free(&host->header_host_signature);

        /* Free source configuration */
        if (host->config) mk_config_free(host->config);
        mk_mem_free(host);
    }
}
#endif
