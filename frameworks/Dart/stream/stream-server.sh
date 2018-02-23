#!/bin/bash


#
# start dart servers
#
current=9001
end=$(($current+$CPU_COUNT))
while [ $current -lt $end ]; do
  dart server.dart -a 0.0.0.0 -p $current -d ${CPU_COUNT} &
  let current=current+1
done
