#!/bin/bash

fw_depends mysql nim jester nginx

nim c -d:release hello.nim
nginx -c $TROOT/config/nginx.conf

for i in $(seq 1 $(nproc --all)); do
  ./hello &
done
