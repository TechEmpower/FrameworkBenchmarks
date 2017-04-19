#!/bin/bash

fw_depends libevent
gcc -O3 -o server server.c -I$IROOT -levent
./server &
