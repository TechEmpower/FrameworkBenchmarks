#!/bin/bash

for i in $(seq 1 $(nproc --all)); do
  ./server-postgres -p 8080 &
done

wait
