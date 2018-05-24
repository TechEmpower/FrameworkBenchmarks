#!/bin/bash

for i in $(seq 1 $(nproc --all)); do
  ./server_radix.out &
done

wait
