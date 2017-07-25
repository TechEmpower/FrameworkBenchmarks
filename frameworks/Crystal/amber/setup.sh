#!/bin/bash

fw_depends postgresql crystal

shards install --production

crystal build --release src/amber.cr

for i in $(seq 1 $(nproc --all)); do
  ./amber &
done

wait
