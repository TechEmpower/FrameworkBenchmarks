/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

#ifndef _FCGI_ENV_H_
#define _FCGI_ENV_H_

#include <stdint.h>

size_t fcgi_env_write(uint8_t *ptr,
		const size_t len,
		struct client_session *cs,
		struct session_request *sr);

#endif // _FCGI_ENV_H_
