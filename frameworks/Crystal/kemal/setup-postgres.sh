#!/bin/bash

fw_depends postgres crystal

shards install

crystal build --release server-postgres.cr

./server-postgres &
