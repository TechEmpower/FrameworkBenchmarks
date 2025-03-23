#!/bin/sh

for i in $(seq 0 $(nproc --ignore=1)); do
  taskset -c $i ./main &
done

wait
