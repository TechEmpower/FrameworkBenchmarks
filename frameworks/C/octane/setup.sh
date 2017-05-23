#!/bin/bash

fw_depends octane

LD_PRELOAD="$OCTANE_HOME/lib/lockless_allocator/libllalloc.so.1.3" techempower_benchmarks &
