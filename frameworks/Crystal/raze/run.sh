#!/bin/bash

for i in $(seq 1 $(nproc --all)); do
  ./raze -p 8080 &
done

wait
