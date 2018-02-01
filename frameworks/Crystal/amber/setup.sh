#!/bin/bash

fw_depends postgresql crystal

export GC_MARKERS=1

export AMBER_ENV=production

export DATABASE_URL=postgres://benchmarkdbuser:benchmarkdbpass@TFB-database:5432/hello_world

shards build amber --release --no-debug

for i in $(seq 1 $(nproc --all)); do
  ./bin/amber &
done

wait
