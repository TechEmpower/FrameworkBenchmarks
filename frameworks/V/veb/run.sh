#!/bin/sh

for i in $(seq 0 $(nproc)); do
  taskset -c $i ./main &
done

wait
