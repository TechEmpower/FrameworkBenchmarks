#!/bin/bash

for i in $(seq 1 $(nproc --all)); do
  ./server.out &
done

wait
