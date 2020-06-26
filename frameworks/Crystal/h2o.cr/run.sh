#!/bin/sh

for i in $(seq 1 $(nproc --all)); do
  ./server.out &
done

wait
