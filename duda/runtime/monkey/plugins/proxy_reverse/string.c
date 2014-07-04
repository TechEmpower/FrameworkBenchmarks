/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2013, Nikola Nikov
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

#include <stdlib.h>
#include <string.h>

#include "types.h"

struct string *string_alloc(const char *data, size_t length)
{
    struct string *result =
        mk_api->mem_alloc(sizeof(struct string) +
                          sizeof(char) * (length + 1));
    if (!result) {
        return 0;
    }
    result->data = (char *) (result + 1);
    result->length = length;
    if (data) {
        memcpy(result->data, data, length);
    }
    result->data[length] = 0;
    return result;
}
