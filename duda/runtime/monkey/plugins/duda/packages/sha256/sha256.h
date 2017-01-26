/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Duda I/O
 *  --------
 *  Copyright (C) 2001-2012, Eduardo Silva P. <edsiper@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef DUDA_PACKAGE_SHA256_H
#define DUDA_PACKAGE_SHA256_H

struct duda_api_sha256 {
  void (*encode) (unsigned char *, unsigned char *, int);
};

typedef struct duda_api_sha256 sha256_object_t;
sha256_object_t *sha256;

#ifndef uint8
#define uint8  unsigned char
#endif

#ifndef uint32
#define uint32 unsigned long int
#endif

typedef struct
{
    uint32 total[2];
    uint32 state[8];
    uint8 buffer[64];
}
sha256_context;

void sha256_starts(sha256_context *ctx);
void sha256_update(sha256_context *ctx, uint8 *input, uint32 length);
void sha256_finish(sha256_context *ctx, uint8 digest[32]);

#endif


