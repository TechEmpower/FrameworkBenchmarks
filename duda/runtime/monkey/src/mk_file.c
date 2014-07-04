/*-*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

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

#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "monkey.h"
#include "mk_file.h"
#include "mk_user.h"
#include "mk_memory.h"
#include "mk_utils.h"
#include "mk_macros.h"

int mk_file_get_info(const char *path, struct file_info *f_info)
{
    struct stat f, target;

    f_info->exists = MK_FALSE;

    /* Stat right resource */
    if (lstat(path, &f) == -1) {
        if (errno == EACCES) {
            f_info->exists = MK_TRUE;
        }
        return -1;
    }

    f_info->exists = MK_TRUE;
    f_info->is_file = MK_TRUE;
    f_info->is_link = MK_FALSE;
    f_info->is_directory = MK_FALSE;
    f_info->exec_access = MK_FALSE;
    f_info->read_access = MK_FALSE;

    if (S_ISLNK(f.st_mode)) {
        f_info->is_link = MK_TRUE;
        f_info->is_file = MK_FALSE;
        if (stat(path, &target) == -1) {
            return -1;
        }
    }
    else {
        target = f;
    }

    f_info->size = target.st_size;
    f_info->last_modification = target.st_mtime;

    if (S_ISDIR(target.st_mode)) {
        f_info->is_directory = MK_TRUE;
        f_info->is_file = MK_FALSE;
    }

    /* Checking read access */
    if (((target.st_mode & S_IRUSR) && target.st_uid == EUID) ||
        ((target.st_mode & S_IRGRP) && target.st_gid == EGID) ||
        (target.st_mode & S_IROTH)) {
        f_info->read_access = MK_TRUE;
    }
#ifdef TRACE
    else {
        MK_TRACE("Target has not read acess");
    }
#endif

    /* Checking execution access */
    if ((target.st_mode & S_IXUSR && target.st_uid == EUID) ||
        (target.st_mode & S_IXGRP && target.st_gid == EGID) ||
        (target.st_mode & S_IXOTH)) {
        f_info->exec_access = MK_TRUE;

    }
#ifdef TRACE
    else {
        MK_TRACE("warning: target has not execution permission");
    }
#endif

    /* Suggest open(2) flags */
    f_info->flags_read_only = O_RDONLY | O_NONBLOCK;

    /*
     * If the user is the owner of the file or the user is root, it
     * can set the O_NOATIME flag for open(2) operations to avoid
     * inode updates about last accessed time
     */
    if (target.st_uid == EUID || EUID == 0) {
        f_info->flags_read_only |=  O_NOATIME;
    }
    return 0;
}

/* Read file content to a memory buffer,
 * Use this function just for really SMALL files
 */
char *mk_file_to_buffer(const char *path)
{
    FILE *fp;
    char *buffer;
    long bytes;
    struct file_info finfo;

    if (mk_file_get_info(path, &finfo) != 0) {
        return NULL;
    }

    if (!(fp = fopen(path, "r"))) {
        return NULL;
    }

    buffer = mk_mem_malloc_z(finfo.size + 1);
    if (!buffer) {
        fclose(fp);
        return NULL;
    }

    bytes = fread(buffer, finfo.size, 1, fp);

    if (bytes < 1) {
        mk_mem_free(buffer);
        fclose(fp);
        return NULL;
    }

    fclose(fp);
    return (char *) buffer;

}
