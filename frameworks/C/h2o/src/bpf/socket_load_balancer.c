/*
 Copyright (c) 2025 Anton Valentinov Kirilov

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// TODO: Switch to the standard atomics (<stdatomic.h>) after
// the system header file mess gets sorted for eBPF.
#include <stdbool.h>
#include <stddef.h>
#include <linux/bpf.h>
#include <bpf/bpf_helpers.h>

// We need a finite number of iterations to keep the eBPF verifier happy.
#define MAX_ITERATIONS 42

static size_t thread_idx;
size_t thread_num = 1;

SEC("socket") int socket_load_balancer(void *skb)
{
	(void) skb;

	// TODO: Use __atomic_load_n() after LLVM starts supporting it for eBPF.
	size_t idx = *(const volatile size_t *) &thread_idx;
	int ret = thread_num;

	__atomic_thread_fence(__ATOMIC_RELAXED);

	for (size_t i = 0; i < MAX_ITERATIONS; i++) {
		const size_t new_idx = (idx + 1) % thread_num;

		if (__atomic_compare_exchange_n(&thread_idx,
		                                &idx,
		                                new_idx,
		                                false,
		                                __ATOMIC_RELAXED,
		                                __ATOMIC_RELAXED)) {
			ret = idx;
			break;
		}
	}

	return ret;
}
