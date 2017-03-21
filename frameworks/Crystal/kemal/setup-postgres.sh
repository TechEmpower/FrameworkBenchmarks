#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release server-postgres.cr

./server-postgres &
