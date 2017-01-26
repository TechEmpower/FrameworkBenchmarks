/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2012-2014, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include <string.h>
#include "MKPlugin.h"

#include "duda_conf.h"

int duda_conf_set_confdir(struct web_service *ws, const char *dir)
{
    int ret;
    int len;
    struct file_info finfo;

    ret = mk_api->file_get_info(dir, &finfo);
    if (ret != 0 || finfo.is_directory != MK_TRUE) {
        return -1;
    }

    if (ws->confdir.data) {
        free(ws->confdir.data);
    }

    len = strlen(dir);
    if (dir[len - 1] != '/') {
        ws->confdir.data = mk_api->mem_alloc(len + 2);
        strncpy(ws->confdir.data, dir, len);
        ws->confdir.data[len]     = '/';
        ws->confdir.data[len + 1] = '\0';
        ws->confdir.len           = len + 2;
    }
    else {
        ws->confdir.data = mk_api->str_dup(dir);
        ws->confdir.len  = len;
    }

    return 0;
}

int duda_conf_set_datadir(struct web_service *ws, const char *dir)
{
    int ret;
    int len;
    struct file_info finfo;

    ret = mk_api->file_get_info(dir, &finfo);
    if (ret != 0 || finfo.is_directory != MK_TRUE) {
        return -1;
    }

    if (ws->datadir.data) {
        free(ws->datadir.data);
    }

    len = strlen(dir);
    if (dir[len - 1] != '/') {
        ws->datadir.data = mk_api->mem_alloc(len + 2);
        strncpy(ws->datadir.data, dir, len);
        ws->datadir.data[len]     = '/';
        ws->datadir.data[len + 1] = '\0';
        ws->datadir.len           = len + 2;
    }
    else {
        ws->datadir.data = mk_api->str_dup(dir);
        ws->datadir.len  = len;
    }

    return 0;
}

int duda_conf_set_logdir(struct web_service *ws, const char *dir)
{
    int ret;
    int len;
    struct file_info finfo;

    ret = mk_api->file_get_info(dir, &finfo);
    if (ret != 0 || finfo.is_directory != MK_TRUE) {
        return -1;
    }

    if (ws->logdir.data) {
        free(ws->logdir.data);
    }

    len = strlen(dir);
    if (dir[len - 1] != '/') {
        ws->logdir.data = mk_api->mem_alloc(len + 2);
        strncpy(ws->logdir.data, dir, len);
        ws->logdir.data[len]     = '/';
        ws->logdir.data[len + 1] = '\0';
        ws->logdir.len           = len + 2;
    }
    else {
        ws->logdir.data = mk_api->str_dup(dir);
        ws->logdir.len  = len;
    }

    return 0;
}

int duda_conf_main_init(const char *confdir)
{
    int ret = 0;
    unsigned long len;
    char *tmp;
    char *conf_path = NULL;
    struct mk_config_section *section;
    struct mk_config *conf;
    struct file_info finfo;
    struct mk_list *head;

    /* Read Duda configuration file */
    mk_api->str_build(&conf_path, &len, "%s/duda.conf", confdir);
    conf = mk_api->config_create(conf_path);

    mk_list_foreach(head, &conf->sections) {
        section = mk_list_entry(head, struct mk_config_section, _head);
        if (strcasecmp(section->name, "DUDA") != 0) {
            continue;
        }

        /* ServicesRoot */
        services_root = mk_api->config_section_getval(section, "ServicesRoot",
                                                      MK_CONFIG_VAL_STR);

        if (mk_api->file_get_info(services_root, &finfo) != 0) {
            mk_err("Duda: Invalid services root path");
            exit(EXIT_FAILURE);
        }

        if (finfo.is_directory == MK_FALSE) {
            mk_err("Duda: ServicesRoot must be a valid directory");
            exit(EXIT_FAILURE);
        }

        /* Packages */
        packages_root = mk_api->config_section_getval(section, "PackagesRoot",
                                                      MK_CONFIG_VAL_STR);
        if (mk_api->file_get_info(packages_root, &finfo) != 0) {
            mk_err("Duda: Invalid packages root path");
            exit(EXIT_FAILURE);
        }

        if (finfo.is_directory == MK_FALSE) {
            mk_err("Duda: PackagesRoot must be a valid directory");
            exit(EXIT_FAILURE);
        }

        /* Duda Document Root (aka '/ddr') */
        tmp = mk_api->config_section_getval(section, "DocumentRoot",
                                            MK_CONFIG_VAL_STR);
        if (tmp) {
            document_root.data = tmp;
            document_root.len  = strlen(tmp);
        }
        else {
            document_root.data = NULL;
            document_root.len  = 0;
        }

        if (mk_api->file_get_info(packages_root, &finfo) != 0) {
            mk_err("Duda: Invalid document root path");
            exit(EXIT_FAILURE);
        }

        if (finfo.is_directory == MK_FALSE) {
            mk_err("Duda: DocumentRoot must be a valid directory");
            exit(EXIT_FAILURE);
        }

        PLUGIN_TRACE("Services Root '%s'", services_root);
        PLUGIN_TRACE("Packages Root '%s'", packages_root);
    }

    mk_api->mem_free(conf_path);

    return ret;
}

int duda_conf_vhost_init()
{
    int ret;
    int len;

    /* Section data */
    char *app_name;
    char *app_docroot;
    char *app_confdir;
    char *app_datadir;
    char *app_logdir;
    int   app_enabled;
    int   app_is_root;

    struct file_info finfo;

    /* vhost services list */
    struct vhost_services *vs;

    /* web service details */
    struct web_service *ws;

    /* monkey vhost configuration */
    struct mk_list *head_host;
    struct mk_list *hosts = &mk_api->config->hosts;
    struct mk_list *head_section;
    struct host *entry_host;
    struct mk_config_section *section;

    mk_list_init(&services_list);
    mk_list_init(&services_loaded);

    PLUGIN_TRACE("Loading applications");
    mk_list_foreach(head_host, hosts) {
        entry_host = mk_list_entry(head_host, struct host, _head);

        vs = mk_api->mem_alloc(sizeof(struct vhost_services));
        vs->host         = entry_host;      /* link virtual host entry     */
        vs->root_service = NULL;            /* root web service (optional) */
        mk_list_init(&vs->services);        /* init services list          */

        /*
         * check vhost 'config' and look for [WEB_SERVICE] sections, we don't use
         * mk_config_section_get() because we can have multiple [WEB_SERVICE]
         * sections.
         */
        mk_list_foreach(head_section, &entry_host->config->sections) {
            section = mk_list_entry(head_section, struct mk_config_section, _head);

            if (strcasecmp(section->name, "WEB_SERVICE") == 0) {
                /* Initialize temporal keys */
                app_name    = NULL;
                app_enabled = MK_FALSE;
                app_is_root = MK_FALSE;
                app_docroot = NULL;
                app_confdir = NULL;
                app_logdir  = NULL;

                /* Get section keys */
                app_name = mk_api->config_section_getval(section,
                                                         "Name",
                                                         MK_CONFIG_VAL_STR);

                app_enabled = (size_t) mk_api->config_section_getval(section,
                                                                     "Enabled",
                                                                     MK_CONFIG_VAL_BOOL);

                app_is_root = (size_t) mk_api->config_section_getval(section,
                                                                     "Root",
                                                                     MK_CONFIG_VAL_BOOL);

                app_docroot = mk_api->config_section_getval(section,
                                                            "DocumentRoot",
                                                            MK_CONFIG_VAL_STR);

                app_confdir = mk_api->config_section_getval(section,
                                                            "ConfDir",
                                                            MK_CONFIG_VAL_STR);

                app_datadir = mk_api->config_section_getval(section,
                                                            "DataDir",
                                                            MK_CONFIG_VAL_STR);

                app_logdir = mk_api->config_section_getval(section,
                                                           "LogDir",
                                                           MK_CONFIG_VAL_STR);

                if (app_name && mk_is_bool(app_enabled)) {
                    ws = mk_api->mem_alloc_z(sizeof(struct web_service));

                    /* fixed name */
                    ws->fixed_name.data = mk_api->str_dup(app_name);
                    ws->fixed_name.len  = strlen(app_name);

                    /*
                     * name: we duplicate this from fixed name as it later can be
                     * changed from the web service through conf->service_name()
                     */
                    ws->name.data = mk_api->str_dup(app_name);
                    ws->name.len  = strlen(app_name);

                    /* enable */
                    ws->enabled = app_enabled;

                    /* Disable map root by default */
                    ws->map_root_name = NULL;

                    /* document root */
                    if (app_docroot) {
                        ret = mk_api->file_get_info(app_docroot, &finfo);
                        if (ret != 0 || finfo.is_directory != MK_TRUE) {
                            mk_err("Duda: invalid DocumentRoot, it must be a directory");
                            exit(EXIT_FAILURE);
                        }

                        len = strlen(app_docroot);
                        if (app_docroot[len - 1] != '/') {
                            ws->docroot.data = mk_api->mem_alloc(len + 2);
                            strncpy(ws->docroot.data, app_docroot, len);
                            ws->docroot.data[len]    = '/';
                            ws->docroot.data[len + 1]= '\0';
                            ws->docroot.len  = len + 1;
                        }
                        else {
                            ws->docroot.data = mk_api->str_dup(app_docroot);
                            ws->docroot.len  = len;
                        }
                    }

                    /* ConfDir */
                    if (app_confdir) {
                        ret = duda_conf_set_confdir(ws, app_confdir);
                        if (ret != 0) {
                            mk_err("Duda: invalid ConfDir, it must be a directory");
                            exit(EXIT_FAILURE);
                        }
                    }

                    /* DataDir */
                    if (app_datadir) {
                        ret = duda_conf_set_datadir(ws, app_datadir);
                        if (ret != 0) {
                            mk_err("Duda: invalid DataDir, it must be a directory");
                            exit(EXIT_FAILURE);
                        }
                    }

                    /* LogDir */
                    if (app_logdir) {
                        ret = duda_conf_set_logdir(ws, app_logdir);
                        if (ret != 0) {
                            mk_err("Duda: invalid LogDir, it must be a directory");
                            exit(EXIT_FAILURE);
                        }
                    }

                    /*
                     * If this web service wants to be Root (the only one who serves
                     * everything on this virtual host, let the parent head know about
                     * it.
                     */
                    if (app_is_root == MK_TRUE) {
                        ws->is_root = MK_TRUE;
                        vs->root_service = ws;
                    }
                    else {
                        ws->is_root = MK_FALSE;
                    }

                    /* Set a reference to the parent vhost_services */
                    ws->vh_parent = vs;

                    /* link data to the web services list */
                    mk_list_add(&ws->_head, &vs->services);
                    mk_list_add(&ws->_head_loaded, &services_loaded);
                }
                else {
                    mk_warn("Duda: Invalid web service, skipping");
                }
            }
        }

        /* Link web_service node to global list services_list */
        mk_list_add(&vs->_head, &services_list);
    }

#ifdef TRACE
    struct mk_list *list_head, *service_head;
    struct vhost_services *service_entry;
    struct web_service *ws_entry;

    mk_list_foreach(list_head, &services_list) {
        service_entry = mk_list_entry(list_head, struct vhost_services, _head);
        PLUGIN_TRACE("Duda Web Service VHost: %p", service_entry->host);

        mk_list_foreach(service_head, &service_entry->services) {
            ws_entry = mk_list_entry(service_head, struct web_service, _head);
            PLUGIN_TRACE("---");
            PLUGIN_TRACE(" app_name    : %s", ws_entry->name.data);
            PLUGIN_TRACE(" app_enabled : %i", ws_entry->enabled);
        }
    }
#endif

    return 0;
}

void duda_conf_messages_to(struct web_service *ws)
{
    int buf_size = 1024;
    char path[buf_size];
    FILE *f;

    time_t now;
    struct tm *current;

    snprintf(path, buf_size, "/tmp/%s.duda.messages",
             ws->name.data);

    f = freopen(path, "a+", stdout);
    if (!f) {
        perror("freopen");
        return;
    }

    f = freopen(path, "a+", stderr);
    if (!f) {
        perror("freopen");
        return;
    }

    now = time(NULL);
    current = localtime(&now);
    printf("[%i/%02i/%02i %02i:%02i:%02i] Duda I/O > '%s' Started\n",
           current->tm_year + 1900,
           current->tm_mon,
           current->tm_mday,
           current->tm_hour,
           current->tm_min,
           current->tm_sec,
           ws->name.data);
    printf("   version          : %s\n", VERSION);
    printf("   server port      : %i\n", mk_api->config->serverport);
    printf("   number of workers: %i\n", mk_api->config->workers);
    fflush(stdout);

}

/*
 * Methods available for web services through the Object API
 * =========================================================
 */

/*
 * @OBJ_NAME: conf
 * @OBJ_MENU: Configuration
 * @OBJ_DESC: The configuration object provides a set of methods to perform a hard
 * setup of the web services or change the framework behavior. All methods
 * available must be invoked from inside duda_main().
 */


/*
 * @METHOD_NAME: force_redirect
 * @METHOD_DESC: Instruct the web service to perform an HTTP redirection
 * over the requested URIs who do not contain an ending slash.
 * @METHOD_PROTO: void foce_redirect()
 * @METHOD_RETURN: This method do not return any value.
 */
void duda_conf_force_redirect(struct web_service *ws)
{
    ws->url_force_redirect = MK_TRUE;
};

/*
 * @METHOD_NAME: bind_messages
 * @METHOD_DESC: Every time the HTTP stack prints out some text messages, these are
 * sent by default to STDOUT. But if the service is running in background mode those
 * messages will be lost. When invoking this method, it instruct Duda core to redirect
 * all STDOUT messages to the service console file.
 * @METHOD_PROTO: void bind_messages()
 * @METHOD_RETURN: This method do not return any value.
 */

void duda_conf_bind_messages(struct web_service *ws)
{
    ws->bind_messages = MK_TRUE;
}

/*
 * @METHOD_NAME: service_name
 * @METHOD_DESC: It overrides the default web service name inside the stack, this
 * will affect the URI starter string to identify the access to the service. If the
 * service name is 'abc' it usually is accessed by URI /abc, but using this method
 * it will override, e.g: setting service_name('efg') will map now URI /efg.
 * @METHOD_PROTO: void service_name(const char *name)
 * @METHOD_RETURN: This method do not return any value.
 */
void duda_conf_service_name(struct web_service *ws, const char *name)
{
    ws->name.data = mk_api->str_dup(name);
    ws->name.len  = strlen(name);
}

/*
 * @METHOD_NAME: service_root
 * @METHOD_DESC: By default when starting a web service, the way to access it is
 * through it shortname in the URL. Defining a web service as Root, means that it
 * owns a Virtual Host, so all further incoming request arriving to the Virtual
 * Host will be handled by the web service in question. On using this call, the
 * service will be accessed through the root URI '/' (the short name prefix is
 * not longer required).
 * @METHOD_PROTO: void service_root()
 * @METHOD_RETURN: This method do not return any value.
 */
void duda_conf_service_root(struct web_service *ws)
{
    ws->is_root = MK_TRUE;
    ws->vh_parent->root_service = ws;
}

struct duda_api_conf *duda_conf_object()
{
    struct duda_api_conf *c;

    c = mk_api->mem_alloc(sizeof(struct duda_api_conf));
    c->_force_redirect  = duda_conf_force_redirect;
    c->_bind_messages   = duda_conf_bind_messages;
    c->_service_name    = duda_conf_service_name;
    c->_service_root    = duda_conf_service_root;

    return c;
}
