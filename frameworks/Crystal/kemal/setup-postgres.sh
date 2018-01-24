#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release --no-debug server-postgres.cr

export GC_MARKERS=1

export KEMAL_ENV=production

for i in $(seq 1 $(nproc --all)); do
  ./server-postgres -p 8080 &
done

wait
