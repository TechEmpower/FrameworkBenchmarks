#!/bin/bash

let max_threads=$(cat /proc/cpuinfo | grep processor | wc -l)
echo ""
echo "---------------------------------------------------------"
echo " Running Primer $name"
echo " wrk -H 'Host: $server_host' -H 'Accept: $accept' -H 'Connection: keep-alive' --latency -d 5 -c 8 --timeout 8 -t 8 \"${url}2\""
echo "---------------------------------------------------------"
echo ""
wrk -H "Host: $server_host" -H "Accept: $accept" -H "Connection: keep-alive" --latency -d 5 -c 8 --timeout 8 -t 8 "${url}2"
sleep 5

echo ""
echo "---------------------------------------------------------"
echo " Running Warmup $name"
echo " wrk -H 'Host: $server_host' -H 'Accept: $accept' -H 'Connection: keep-alive' --latency -d $duration -c $max_concurrency --timeout 8 -t $max_threads \"${url}2\""
echo "---------------------------------------------------------"
echo ""
wrk -H "Host: $server_host" -H "Accept: $accept" -H "Connection: keep-alive" --latency -d $duration -c $max_concurrency --timeout 8 -t $max_threads "${url}2"
sleep 5

for c in $levels
do
echo ""
echo "---------------------------------------------------------"
echo " Queries: $c for $name"
echo " wrk -H 'Host: $server_host' -H 'Accept: $accept' -H 'Connection: keep-alive' --latency -d $duration -c $max_concurrency --timeout 8 -t $max_threads \"$url$c\""
echo "---------------------------------------------------------"
echo ""
STARTTIME=$(date +"%s")
/usr/bin/time -o elapsed --format="STARTTIME 0\nENDTIME %e" wrk -H "Host: $server_host" -H "Accept: $accept" -H "Connection: keep-alive" --latency -d $duration -c $max_concurrency --timeout 8 -t $max_threads "$url$c"
ENDTIME=$(cat elapsed)
ENDTIME=$(echo "$ENDTIME + $STARTTIME" | bc)
echo "STARTTIME $STARTTIME"
echo "ENDTIME $ENDTIME"
sleep 2
done
