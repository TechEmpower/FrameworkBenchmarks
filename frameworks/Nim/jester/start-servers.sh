#!/bin/bash

nim c -d:release hello.nim
nginx -c /config/nginx.conf

current=9000
end=9008
while [ $current -lt $end ]; do
  ./hello $current &
  let current=current+1
done

wait
