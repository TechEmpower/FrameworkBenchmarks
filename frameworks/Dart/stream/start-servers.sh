#!/bin/bash

#
# start dart servers
#
CPU_COUNT=$(nproc)
current=9001
end=$(($current+$CPU_COUNT))
while [ $current -lt $end ]; do
  dart server.dart -a 127.0.0.1 -p $current -d ${CPU_COUNT} &
  let current=current+1
done
