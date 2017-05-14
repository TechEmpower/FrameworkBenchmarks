#!/bin/bash

fw_depends postgresql crystal

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' config/database.yml

shards install

crystal build --release src/kemalyst.cr

for i in $(seq 1 $(nproc --all)); do
  ./kemalyst &
done

wait
