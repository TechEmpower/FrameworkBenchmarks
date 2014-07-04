#include <libmonkey.h>

// Xfail, stop without start

int main() {

	mklib_ctx c = mklib_init(NULL, 0, 0, NULL);
	if (!c) return 1;

	if (!mklib_stop(c)) return 1;

	return 0;
}
