#include <libmonkey.h>

// Start with a wrong IP. Expected to fail

int main() {

	mklib_ctx c = mklib_init("lol", 0, 0, NULL);
	if (!c) return 1;

	return 0;
}
