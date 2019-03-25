#!/bin/bash

let max_threads=$(cat /proc/cpuinfo | grep processor | wc -l)
echo ""
echo "---------------------------------------------------------"
echo " Running Primer $name"
echo " wrk -H 'Host: $server_host' -H 'Accept: $accept' -H 'Connection: keep-alive' --latency -d 5 -c 8 --timeout 8 -t 8 $url"
echo "---------------------------------------------------------"
echo ""
wrk -H "Host: $server_host" -H "Accept: $accept" -H "Connection: keep-alive" --latency -d 5 -c 8 --timeout 8 -t 8 $url
sleep 5

echo ""
echo "---------------------------------------------------------"
echo " Running Warmup $name"
echo " wrk -H 'Host: $server_host' -H 'Accept: $accept' -H 'Connection: keep-alive' --latency -d $duration -c $max_concurrency --timeout 8 -t $max_threads \"$url\""
echo "---------------------------------------------------------"
echo ""
wrk -H "Host: $server_host" -H "Accept: $accept" -H "Connection: keep-alive" --latency -d $duration -c $max_concurrency --timeout 8 -t $max_threads $url
sleep 5

for c in $levels
do
echo ""
echo "---------------------------------------------------------"
echo " Concurrency: $c for $name"
echo " wrk -H 'Host: $server_host' -H 'Accept: $accept' -H 'Connection: keep-alive' --latency -d $duration -c $c --timeout 8 -t $(($c>$max_threads?$max_threads:$c)) \"$url\""
echo "---------------------------------------------------------"
echo ""
STARTTIME=$(date +"%s")
wrk -H "Host: $server_host" -H "Accept: $accept" -H "Connection: keep-alive" --latency -d $duration -c $c --timeout 8 -t "$(($c>$max_threads?$max_threads:$c))" $url
echo "STARTTIME $STARTTIME"
echo "ENDTIME $(date +"%s")"
sleep 2
done
