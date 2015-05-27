#!/bin/bash
source ${IROOT}/crystal-0.7.1.installed

crystal build --release server.cr -o server.out

./server.out
