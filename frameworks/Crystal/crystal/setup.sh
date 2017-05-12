#!/bin/bash

fw_depends crystal

crystal build --release server.cr -o server.out

for i in $(seq 1 $(nproc --all)); do
  ./server.out &
done

wait
