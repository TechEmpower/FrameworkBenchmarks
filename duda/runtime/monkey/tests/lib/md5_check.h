#ifndef MD5_CHECK_H
#define MD5_CHECK_H

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int md5_check(const char *right, const char *where) {

	char *def, buf[33];
	asprintf(&def, "wget --timeout=2 -t2 -q -O- %s | md5sum -", where);

	FILE *f = popen(def, "r");
	if (!f) return 0;

	fgets(buf, 33, f);

	free(def);
	pclose(f);

	return (strcmp(right, buf) == 0);
}

#endif
