#!/bin/bash

fw_depends crystal

crystal deps install

crystal build --release server-redis.cr

./server-redis -e production &
