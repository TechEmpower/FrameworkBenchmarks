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

#ifndef MK_FILE_H
#define MK_FILE_H

struct file_info
{
    off_t size;
    time_t last_modification;

    /* Suggest flags to open this file */
    int flags_read_only;

    unsigned char exists;
    unsigned char is_file;
    unsigned char is_link;
    unsigned char is_directory;
    unsigned char exec_access;
    unsigned char read_access;
};

int mk_file_get_info(const char *path, struct file_info *f_info);
char *mk_file_to_buffer(const char *path);

#endif
