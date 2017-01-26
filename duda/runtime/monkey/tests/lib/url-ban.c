#include <libmonkey.h>
#include <stdlib.h>

// Make sure that an URL-ban works


int data(const mklib_session *sr, const char *vhost, const char *url,
		const char *get, unsigned long get_len,
		const char *post, unsigned long post_len,
		unsigned int *status, const char **content,
		unsigned long *content_len, char *header) {

	// If we got here, the ban failed.
	exit(1);
}

int urlcheck(const char *ip) {
	// Ban everyone.
	return MKLIB_FALSE;
}

int main() {

	mklib_ctx c = mklib_init(NULL, 8090, 0, NULL);
	if (!c) return 1;

	mklib_callback_set(c, MKCB_URLCHECK, urlcheck);
	mklib_callback_set(c, MKCB_DATA, data);

	if (!mklib_start(c)) return 1;

	system("wget -q -t2 -O /dev/null localhost:8090");

	if (!mklib_stop(c)) return 1;

	return 0;
}
