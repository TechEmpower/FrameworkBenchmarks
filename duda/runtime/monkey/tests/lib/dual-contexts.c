#include <libmonkey.h>
#include "md5_check.h"

// Serve on two ports at once.
// This is known to fail, would require deep surgery to handle properly.

const char data1c[] = "data1";
const char data2c[] = "data2";

int data1(const mklib_session *sr, const char *vhost, const char *url,
		const char *get, unsigned long get_len,
		const char *post, unsigned long post_len,
		unsigned int *status, const char **content,
		unsigned long *content_len, char *header) {

	*content_len = 5;

	*content = data1c;

	return MKLIB_TRUE;
}

int data2(const mklib_session *sr, const char *vhost, const char *url,
		const char *get, unsigned long get_len,
		const char *post, unsigned long post_len,
		unsigned int *status, const char **content,
		unsigned long *content_len, char *header) {

	*content_len = 5;

	*content = data2c;

	return MKLIB_TRUE;
}

int main() {

	mklib_ctx c1 = mklib_init(NULL, 8094, 0, NULL);
	if (!c1) return 1;
	mklib_ctx c2 = mklib_init(NULL, 8095, 0, NULL);
	if (!c2) return 1;

	mklib_callback_set(c1, MKCB_DATA, data1);
	mklib_callback_set(c2, MKCB_DATA, data2);

	if (!mklib_start(c1)) return 1;
	if (!mklib_start(c2)) return 1;

	if (!md5_check("89d903bc35dede724fd52c51437ff5fd", "localhost:8094")) return 1;
	if (!md5_check("ff9cf2d690d888cb337f6bf4526b6130", "localhost:8095")) return 1;

	if (!mklib_stop(c1)) return 1;
	if (!mklib_stop(c2)) return 1;

	return 0;
}
