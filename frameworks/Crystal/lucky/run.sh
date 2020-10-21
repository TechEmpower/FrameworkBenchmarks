#!/bin/bash

for i in $(seq 1 $(nproc --all)); do
  ./bin/bench &
done

wait
