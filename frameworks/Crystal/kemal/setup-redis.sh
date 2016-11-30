#!/bin/bash

fw_depends crystal

shards install

crystal build --release server-redis.cr

./server-redis &
