#ifndef BITAP_H_
#define BITAP_H_
#include <string.h>
#include <limits.h>

inline void *bitap_bitwise_search(const void *text, int textLen, const void *pattern, int patternLen) {
	int m = patternLen;
	unsigned long R;
	unsigned long pattern_mask[CHAR_MAX + 1];
	int i;

	if (m > 31) return NULL;
	/* Initialize the bit array R */
	R = ~1;

	/* Initialize the pattern bitmasks */
	for (i = 0; i <= CHAR_MAX; ++i)
		pattern_mask[i] = ~0;
	for (i = 0; i < m; ++i)
		pattern_mask[int(((char*) pattern)[i])] &= ~(1UL << i);

	for (i = 0; i < textLen; ++i) {
		/* Update the bit array */
		R |= pattern_mask[int(((char*) text)[i])];
		R <<= 1;
		if (0 == (R & (1UL << m))) return ((char*) text + i - m) + 1;
	}
	return NULL;
}

#endif /* BITAP_H_ */
