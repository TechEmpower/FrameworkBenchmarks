#include <libmonkey.h>
#include "md5_check.h"

// Send 50k of zeros, check that md5sum matches

enum {
	clen = 50000
};

int data(const mklib_session *sr, const char *vhost, const char *url,
		const char *get, unsigned long get_len,
		const char *post, unsigned long post_len,
		unsigned int *status, const char **content,
		unsigned long *content_len, char *header) {

	*content_len = clen;

	*content = calloc(clen, 1);

	return MKLIB_TRUE;
}

int main() {

	mklib_ctx c = mklib_init(NULL, 8090, 0, NULL);
	if (!c) return 1;

	mklib_callback_set(c, MKCB_DATA, data);

	if (!mklib_start(c)) return 1;

	if (!md5_check("a68b2482a6c645a9738329909861afb3", "localhost:8090")) return 1;

	if (!mklib_stop(c)) return 1;

	return 0;
}
