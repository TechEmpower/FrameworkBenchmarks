/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 * Base64 encoding/decoding (RFC1341)
 * Copyright (c) 2005, Jouni Malinen <jkmaline@cc.hut.fi>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * Alternatively, this software may be distributed under the terms of BSD
 * license.
 *
 * See README and COPYING for more details.
 */

/*
 * @OBJ_NAME: base64
 * @OBJ_MENU: Base64
 * @OBJ_DESC: The base64 package allows you to encode or decode text using
 * the Base64 algorithm.
 * @PKG_HEADER: #include "packages/base64/base64.h"
 * @PKG_INIT: duda_load_package(base64, "base64");
 */

#include <stdlib.h>
#include <string.h>

#include "duda_package.h"
#include "base64.h"

static const unsigned char base64_table[64] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/**
 * base64_encode - Base64 encode
 * @src: Data to be encoded
 * @len: Length of the data to be encoded
 * @out_len: Pointer to output length variable, or %NULL if not used
 * Returns: Allocated buffer of out_len bytes of encoded data,
 * or %NULL on failure
 *
 * Caller is responsible for freeing the returned buffer. Returned buffer is
 * nul terminated to make it easier to use as a C string. The nul terminator is
 * not included in out_len.
 */

/*
 * @METHOD_NAME: encode
 * @METHOD_DESC: Encode data into Base64 format
 * @METHOD_PROTO: unsigned char *encode(const unsigned char *src, size_t len, size_t *out_len)
 * @METHOD_PARAM: src The data to be encoded
 * @METHOD_PARAM: len Length of the data to be encoded
 * @METHOD_PARAM: out_len Pointer to output length variable, or NULL if not used
 * @METHOD_RETURN: Returns a new allocated buffer containing the data encoded in Base64 format.
 */
unsigned char *base64_encode(const unsigned char *src, size_t len,
                             size_t *out_len)
{
	unsigned char *out, *pos;
	const unsigned char *end, *in;
	size_t olen;
	int line_len;

	olen = len * 4 / 3 + 4; /* 3-byte blocks to 4-byte */
	olen += olen / 72; /* line feeds */
	olen++; /* nul termination */
	out = monkey->mem_alloc(olen);
	if (out == NULL)
		return NULL;

	end = src + len;
	in = src;
	pos = out;
	line_len = 0;
	while (end - in >= 3) {
		*pos++ = base64_table[in[0] >> 2];
		*pos++ = base64_table[((in[0] & 0x03) << 4) | (in[1] >> 4)];
		*pos++ = base64_table[((in[1] & 0x0f) << 2) | (in[2] >> 6)];
		*pos++ = base64_table[in[2] & 0x3f];
		in += 3;
		line_len += 4;
	}

	if (end - in) {
		*pos++ = base64_table[in[0] >> 2];
		if (end - in == 1) {
			*pos++ = base64_table[(in[0] & 0x03) << 4];
			*pos++ = '=';
		} else {
			*pos++ = base64_table[((in[0] & 0x03) << 4) |
					      (in[1] >> 4)];
			*pos++ = base64_table[(in[1] & 0x0f) << 2];
		}
		*pos++ = '=';
		line_len += 4;
	}

	if (line_len)
		*pos++ = '\n';

	*pos = '\0';
	if (out_len)
		*out_len = pos - out;

    out[*out_len - 1] = '\0';
	return out;
}

/**
 * base64_decode - Base64 decode
 * @src: Data to be decoded
 * @len: Length of the data to be decoded
 * @out_len: Pointer to output length variable
 * Returns: Allocated buffer of out_len bytes of decoded data,
 * or %NULL on failure
 *
 * Caller is responsible for freeing the returned buffer.
 */

/*
 * @METHOD_NAME: decode
 * @METHOD_DESC: Decode Base64 data
 * @METHOD_PROTO: unsigned char *decode(const unsigned char *src, size_t len, size_t *out_len)
 * @METHOD_PARAM: src The data to be decoded
 * @METHOD_PARAM: len Length of the data to be decoded
 * @METHOD_PARAM: out_len Pointer to output length variable.
 * @METHOD_RETURN: Returns a new allocated buffer of out_len bytes of decoded data, or NULL on failure
 */
unsigned char *base64_decode(const unsigned char *src, size_t len,
                              size_t *out_len)
{
    unsigned char dtable[256], *out, *pos, in[4], block[4], tmp;
    size_t i, count;

    memset(dtable, 0x80, 256);
    for (i = 0; i < sizeof(base64_table); i++)
        dtable[base64_table[i]] = i;
    dtable['='] = 0;

    count = 0;
    for (i = 0; i < len; i++) {
        if (dtable[src[i]] != 0x80)
            count++;
    }

    if (count % 4)
        return NULL;

    pos = out = monkey->mem_alloc(count);
    if (out == NULL)
        return NULL;

    count = 0;
    for (i = 0; i < len; i++) {
        tmp = dtable[src[i]];
        if (tmp == 0x80)
            continue;

        in[count] = src[i];
        block[count] = tmp;
        count++;
        if (count == 4) {
            *pos++ = (block[0] << 2) | (block[1] >> 4);
            *pos++ = (block[1] << 4) | (block[2] >> 2);
            *pos++ = (block[2] << 6) | block[3];
            count = 0;
        }
    }

    if (pos > out) {
        if (in[2] == '=')
            pos -= 2;
        else if (in[3] == '=')
            pos--;
    }

    *out_len = pos - out;
    return out;
}
