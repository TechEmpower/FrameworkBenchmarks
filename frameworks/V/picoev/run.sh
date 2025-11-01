#!/bin/sh

for i in $(seq 0 $(($(nproc --all)-1))); do
  taskset -c $i ./picoev &
done

wait
