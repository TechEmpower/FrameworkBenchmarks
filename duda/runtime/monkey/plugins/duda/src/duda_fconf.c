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

#include "duda.h"
#include "duda_conf.h"
#include "duda_fconf.h"

/*
 * @OBJ_NAME: fconf
 * @OBJ_MENU: File Configuration
 * @OBJ_DESC: It provides a set of methods to handle specific configuration files
 * for the web service in question. Its mandatory that the configuration key
 * 'ConfDir' exists under the [WEB_SERVICE] section for the virtual host where
 * this service is running. You can also define configuration directory using the
 * method fconf->set_path().
 */


/*
 * @METHOD_NAME: set_path
 * @METHOD_DESC: It specify a new configuration directory for the web service. It must be a valid path. If it fails it will continue using the configuration directory set on the web server virtual host definition.
 * @METHOD_PROTO: int set_path(const char *dir)
 * @METHOD_PARAM: dir directory path where the configuration files are located.
 * @METHOD_RETURN: Upon successful completion it returns 0, on error returns -1.
 */
int duda_fconf_set_path(struct web_service *ws, const char *dir)
{
    int ret;

    ret = duda_conf_set_confdir(ws, dir);
    if (ret == -1) {
        return -1;
    }

    return 0;
};

/*
 * @METHOD_NAME: get_path
 * @METHOD_DESC: It returns the configuration directory being used by the web service.
 * @METHOD_PROTO: const char *get_path()
 * @METHOD_RETURN: Upon successful completion it returns the directory path, on error or if the path is not set returns NULL.
 */
const char *duda_fconf_get_path(struct web_service *ws)
{
    return ws->confdir.data;
};

/*
 * @METHOD_NAME: read_conf
 * @METHOD_DESC: Locate a named file under the web service configuration directory , read it and return a buffer with it contents.
 * @METHOD_PROTO: struct duda_config *read_conf(const char *path)
 * @METHOD_RETURN: Upon successful completion it returns the duda_config context,
 * on error it returns NULL.
 */
struct duda_config *duda_fconf_read_conf(struct web_service *ws, const char *path)
{
    unsigned long len;
    char *tmp = NULL;
    struct file_info finfo;
    struct duda_config *cnf;

    /* Compose full path */
    mk_api->str_build(&tmp, &len, "%s/%s", ws->confdir.data, path);
    if (mk_api->file_get_info(tmp, &finfo) == -1) {
        mk_api->mem_free(tmp);
        return NULL;
    }


    if (finfo.is_file == MK_FALSE) {
        mk_api->mem_free(tmp);
        return NULL;
    }

    cnf = (struct duda_config *) mk_api->config_create(tmp);
    mk_api->mem_free(tmp);

    return cnf;
}

/*
 * @METHOD_NAME: free_conf
 * @METHOD_DESC: It release the memory resources used by a duda_config context
 * @METHOD_PROTO: void free_config(struct duda_config *cnf)
 * @METHOD_PARAM: cnf the duda_config context
 * @METHOD_RETURN: this method do not return any value.
 */
void duda_fconf_free_conf(struct duda_config *cnf)
{
    mk_api->config_free((struct mk_config *) cnf);
}

/*
 * @METHOD_NAME: section_get
 * @METHOD_DESC: Retrieve a specific section definition from the configuration
 * context. A section in a duda configuration file is defined within square
 * brackets, e.g: [Section Name].
 * @METHOD_PROTO: struct duda_config_section *section_get(struct duda_config *cnf, const char *name)
 * @METHOD_PARAM: cnf the duda_config context
 * @METHOD_PARAM: name section name
 * @METHOD_RETURN: Uppon successful completion it returns the section context, on error it returns NULL.
 */
struct duda_config_section *duda_fconf_section_get(struct duda_config *cnf, const char *name)
{
    struct duda_config_section *s;

    s = (struct duda_config_section *) mk_api->config_section_get((struct mk_config *) cnf,
                                                                  name);
    return s;
}

/*
 * @METHOD_NAME: section_key
 * @METHOD_DESC: Lookup a specific key inside the section and returns its value
 * @METHOD_PROTO: void *section_key(struct duda_config_section *s, const char *name, int type)
 * @METHOD_PARAM: section section context
 * @METHOD_PARAM: name the name of the key to perform the lookup
 * @METHOD_PARAM: type specify the type of value to return, the available options are: DUDA_CONFIG_STR, DUDA_CONFIG_NUM, DUDA_CONFIG_BOOL and DUDA_CONFIG_LIST.
 * @METHOD_RETURN: Uppon successful completion it returns the section context, on error it returns NULL.
 */
void *duda_fconf_section_key(struct duda_config_section *section,
                             char *name, int type)
{
    return mk_api->config_section_getval((struct mk_config_section *) section,
                                         name, type);
}


/*
 * @METHOD_NAME: read_file
 * @METHOD_DESC: Locate a named file under the file system, read it and return a buffer with it contents.
 * @METHOD_PROTO: char *read_file(const char *path)
 * @METHOD_RETURN: Upon successful completion it returns the buffered file content, on error it returns NULL.
 */
char *duda_fconf_read_file(const char *path)
{
    char *buf;

    struct file_info finfo;

    mk_api->file_get_info(path, &finfo);
    if (finfo.is_file == MK_FALSE) {
        return NULL;
    }

    buf = mk_api->file_to_buffer(path);

    /* FIXME: we need to register this buf pointer into the Garbage Collector,
     * as well add a GC trigger after duda_main() is ran, so the buffer is
     * not longer available. Wondering if developers would like to keep
     * this content in memory.
     */

    return buf;
}

struct duda_api_fconf *duda_fconf_object()
{
    struct duda_api_fconf *c;

    c = mk_api->mem_alloc(sizeof(struct duda_api_fconf));

    /* path */
    c->_get_path = duda_fconf_get_path;
    c->_set_path = duda_fconf_set_path;

    c->read_file = duda_fconf_read_file;

    /* handlers for duda_config */
    c->_read_conf  = duda_fconf_read_conf;
    c->free_conf   = duda_fconf_free_conf;
    c->section_get = duda_fconf_section_get;
    c->section_key = duda_fconf_section_key;

    return c;
}
