#!/bin/bash

fw_depends postgresql crystal

shards install

crystal build --release --no-debug src/amber.cr

export GC_MARKERS=1

export AMBER_ENV=production

export DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@TFB-database:5432/hello_world

for i in $(seq 1 $(nproc --all)); do
  ./amber &
done

wait
