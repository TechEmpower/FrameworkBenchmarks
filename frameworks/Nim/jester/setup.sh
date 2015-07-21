#!/bin/bash

export PATH="$IROOT/nim/bin:$IROOT/nginx/sbin:$PATH"

nim c -d:release hello.nim
nginx -c $TROOT/config/nginx.conf

current=9000
end=9008
while [ $current -lt $end ]; do
  ./hello $current &
  let current=current+1
done
