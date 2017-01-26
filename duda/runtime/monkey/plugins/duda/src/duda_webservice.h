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

#include "MKPlugin.h"

#ifndef DUDA_DWEBSERVICE_H
#define DUDA_DWEBSERVICE_H

struct duda_setup {
    void (*event_signal_cb) (int fd, uint64_t val);
};

/* Web service information */
struct web_service {
    mk_pointer name;       /* web service name                 */
    mk_pointer fixed_name; /* service name given by vhost/file */
    mk_pointer docroot;    /* document root for static content */
    mk_pointer confdir;    /* configuration directory          */
    mk_pointer datadir;    /* data store                       */
    mk_pointer logdir;     /* directory to store logs          */

    int  enabled;
    int  is_root;
    int  url_force_redirect;
    int  bind_messages;

    void *handler;

    /* Routing/Map callbacks */
    char *map_root_name;
    void (*map_root_cb) (void *);

    struct mk_list *map_interfaces;
    struct mk_list *map_urls;

    /* global data */
    struct mk_list *global;

    /* workers list */
    struct mk_list *workers;

    /* loggers list */
    struct mk_list *loggers;

    /* packages loaded by the web service */
    struct mk_list *packages;

    /* generic setup things related to the web service and Duda stack */
    struct duda_setup *setup;

    /* node entry associated with services_list */
    struct mk_list _head;

    /* reference to the parent vhost_services entry */
    struct vhost_services *vh_parent;

    /* node entry associated with services_loaded */
    struct mk_list _head_loaded;

    /* Callbacks */
    void (*exit_cb) ();
};

#endif
