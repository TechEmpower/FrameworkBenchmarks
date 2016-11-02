#!/bin/bash

fw_depends crystal

shards install

crystal build --release server-postgres.cr

./server-postgres &
