/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*  Monkey HTTP Daemon
 *  ------------------
 *  Copyright (C) 2012, Sonny Karlsson
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
#include <stdio.h> /* sscanf */
#include <ctype.h> /* toupper */
#include <sys/socket.h> /* getsockname, getpeername */
#include <arpa/inet.h> /* inet_ntop */

#include "MKPlugin.h"

#include "dbg.h"
#include "protocol.h"

#define __write_param(env, len, pos, key, value) do { \
		check(len - pos > 8 + key.len + value.len, "Out of memory."); \
		pos += fcgi_param_write(env + pos, key, value); \
	} while (0)

size_t fcgi_env_write(uint8_t *ptr,
		const size_t len,
		struct client_session *cs,
		struct session_request *sr)
{
	mk_pointer key, value;
	char buffer[128];
	char *tmpuri = NULL;
	size_t pos = 0;
	struct sockaddr_in addr;
	socklen_t addr_len;
	unsigned int i, j;
	char *hinit, *hend;
	size_t hlen;

	mk_api->pointer_set(&key,   "GATEWAY_INTERFACE");
	mk_api->pointer_set(&value, "CGI/1.1");
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "REDIRECT_STATUS");
	mk_api->pointer_set(&value, "200");
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "SERVER_SOFTWARE");
	mk_api->pointer_set(&value, sr->host_conf->host_signature);
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "DOCUMENT_ROOT");
	value = sr->host_conf->documentroot;
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "SERVER_PROTOCOL");
	value = sr->protocol_p;
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "SERVER_NAME");
	value.data = sr->host_alias->name;
	value.len  = sr->host_alias->len;
	__write_param(ptr, len, pos, key, value);

	addr_len = sizeof(addr);
	if (!getsockname(cs->socket, (struct sockaddr *)&addr, &addr_len)) {
		if (!inet_ntop(AF_INET, &addr.sin_addr, buffer, 128)) {
			log_warn("Failed to get bound address.");
			buffer[0] = '\0';
		}
		mk_api->pointer_set(&key,   "SERVER_ADDR");
		mk_api->pointer_set(&value, buffer);
		__write_param(ptr, len, pos, key, value);

		snprintf(buffer, 128, "%d", ntohs(addr.sin_port));
		mk_api->pointer_set(&key,   "SERVER_PORT");
		mk_api->pointer_set(&value, buffer);
		__write_param(ptr, len, pos, key, value);
	} else {
		log_warn("%s", clean_errno());
		errno = 0;
	}

	mk_api->pointer_set(&key,   "SCRIPT_FILENAME");
	value = sr->real_path;
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "SCRIPT_NAME");
	value = sr->uri_processed;
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "REQUEST_METHOD");
	value = sr->method_p;
	__write_param(ptr, len, pos, key, value);

	addr_len = sizeof(addr);
	if (!getpeername(cs->socket, (struct sockaddr *)&addr, &addr_len)) {
		inet_ntop(AF_INET, &addr.sin_addr, buffer, 128);
		mk_api->pointer_set(&key,   "REMOTE_ADDR");
		mk_api->pointer_set(&value, buffer);
		__write_param(ptr, len, pos, key, value);

		snprintf(buffer, 128, "%d", ntohs(addr.sin_port));
		mk_api->pointer_set(&key,   "REMOTE_PORT");
		mk_api->pointer_set(&value, buffer);
		__write_param(ptr, len, pos, key, value);
	} else {
		log_warn("%s", clean_errno());
		errno = 0;
	}

	mk_api->pointer_set(&key,   "REQUEST_URI");
	if (sr->query_string.len > 0) {
		value.len = sr->uri.len + sr->query_string.len + 2;
		tmpuri = mk_api->mem_alloc(value.len);
		check_mem(tmpuri);
		value.data = tmpuri;
		snprintf(value.data, value.len, "%.*s?%.*s",
			(int)sr->uri.len, sr->uri.data,
			(int)sr->query_string.len, sr->query_string.data);
	} else {
		value = sr->uri;
	}
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "QUERY_STRING");
	value = sr->query_string;
	__write_param(ptr, len, pos, key, value);

	mk_api->pointer_set(&key,   "CONTENT_TYPE");
	value = sr->content_type;
	__write_param(ptr, len, pos, key, value);

	if (sr->content_length > 0) {
		mk_api->pointer_set(&key,   "CONTENT_LENGTH");
		snprintf(buffer, 128, "%d", sr->content_length);
		mk_api->pointer_set(&value, buffer);
		__write_param(ptr, len, pos, key, value);
	}

	if (!strcmp(mk_api->config->transport, MK_TRANSPORT_HTTPS)) {
		mk_api->pointer_set(&key, "HTTPS");
		mk_api->pointer_set(&value, "on");
		__write_param(ptr, len, pos, key, value);
	}

	strcpy(buffer, "HTTP_");

	for (i = 0; i < (unsigned int)sr->headers_toc.length; i++) {
		hinit = sr->headers_toc.rows[i].init;
		hend = sr->headers_toc.rows[i].end;
		hlen = hend - hinit;

		for (j = 0; j < hlen; j++) {
			if (hinit[j] == ':') {
				break;
			}
			else if (hinit[j] != '-') {
				buffer[5 + j] = toupper(hinit[j]);
			}
			else {
				buffer[5 + j] = '_';
			}
		}

		key = (mk_pointer){.len = 5 + j, .data = buffer};
		value = (mk_pointer){.len = hlen - j - 2, .data = hinit + j + 2};

		__write_param(ptr, len, pos, key, value);
	}

	if (tmpuri) mk_api->mem_free(tmpuri);
	return pos;
error:
	if (tmpuri) mk_api->mem_free(tmpuri);
	return pos;
}

#undef __write_param
