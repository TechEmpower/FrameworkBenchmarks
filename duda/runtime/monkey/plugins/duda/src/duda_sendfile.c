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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "MKPlugin.h"
#include "duda_sendfile.h"

struct duda_sendfile *duda_sendfile_new(char *path, off_t offset,
                                        size_t count)
{
    int ret;
    uint64_t fsize;
    struct duda_sendfile *file;

    if (offset < 0) {
        return NULL;
    }

    file = mk_api->mem_alloc(sizeof(struct duda_sendfile));
    file->fd = -1;

    ret = mk_api->file_get_info(path, &file->info);
    if (ret == -1) {
        mk_api->mem_free(file);
        return NULL;
    }

    fsize = file->info.size;
    if ((unsigned) offset > fsize ||
        count > fsize ||
        ((unsigned) offset + count) > fsize) {
        mk_api->mem_free(file);
        return NULL;
    }

    if (file->info.read_access == MK_FALSE) {
        mk_api->mem_free(file);
        return NULL;
    }

    file->fd = open(path, O_RDONLY | O_NONBLOCK);
    if (file->fd < 0) {
        mk_warn("%s:%i %s", __FILE__, __LINE__, strerror(errno));
        mk_api->mem_free(file);
        return NULL;
    }

    file->offset = offset;
    if (count == 0) {
        file->pending_bytes = file->info.size - file->offset;
    }
    else {
        file->pending_bytes = count;
    }

    return file;
}

int duda_sendfile_flush(int socket, struct duda_sendfile *sf)
{
    int bytes;

    bytes = mk_api->socket_send_file(socket, sf->fd,
                                     &sf->offset, sf->pending_bytes);

    if (bytes > 0) {
        sf->pending_bytes -= bytes;
    }
    else if (bytes == -1) {
        sf->pending_bytes = 0;
        return 0;
    }

    return sf->pending_bytes;
}
