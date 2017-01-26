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
#include "duda.h"
#include "duda_conf.h"
#include "duda_package.h"

duda_package_t *duda_package_load(const char *pkgname,
                                  struct duda_api_objects *api,
                                  struct web_service *ws)
{
    int ret;
    char *package = NULL;
    void *handler = NULL;
    unsigned long len;
    struct file_info finfo;
    duda_package_t *(*package_main)() = NULL;
    duda_package_t *package_info;

    mk_api->str_build(&package, &len, "%s/%s.dpkg", packages_root, pkgname);
    ret = mk_api->file_get_info(package, &finfo);

    if (ret != 0) {
        mk_err("Duda: Package '%s' not found", pkgname);
        mk_api->mem_free(package);
        exit(EXIT_FAILURE);
    }

    if (finfo.is_file == MK_FALSE) {
        mk_warn("Duda: Invalid Package '%s'", pkgname);
        mk_api->mem_free(package);
        return NULL;
    }

    handler = duda_load_library(package);
    if (!handler) {
        mk_warn("Duda: Invalid Package format '%s'", pkgname);
        mk_api->mem_free(package);
        return NULL;
    }

    package_main = duda_load_symbol(handler, "_duda_package_bootstrap");
    if (!package_main) {
        mk_err("Duda: the package '%s' is broken", pkgname);
        exit(EXIT_FAILURE);
    }

    package_info = package_main(api, ws);
    package_info->handler = handler;
    mk_api->mem_free(package);

    /* Get global variables
    struct mk_list *a;
    a = duda_load_symbol(handler, "duda_global_dist");
    mk_warn("package '%s' global: %p", pkgname, a);
    */

    return package_info;
}
