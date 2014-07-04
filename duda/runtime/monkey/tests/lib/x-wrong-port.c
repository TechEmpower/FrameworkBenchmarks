#include <libmonkey.h>

// Start with a wrong port. Expected to fail

int main() {

	mklib_ctx c = mklib_init(NULL, -1, 0, NULL);
	if (!c) return 1;

	return 0;
}
