#!/bin/bash

chmod +x $IROOT/ffead-cpp-sql-raw/*.sh

cp $IROOT/ffead-cpp-sql-raw/server.sh /server_orig.sh

cd $IROOT/ffead-cpp-sql-raw
nohup bash -c "./server.sh > ffead.log &"
echo "Waiting for ffead-cpp to launch on port 8080..."
COUNTER=0
while [ ! -f lib/libinter.so ]
do
  sleep 1
  COUNTER=$((COUNTER+1))
  if [ "$COUNTER" = 600 ]
  then
  	cat ffead.log
  	cat logs/jobs.log
    echo "exiting...."
    exit 1
  fi
done
COUNTER=0
while [ ! -f lib/libdinter.so ]
do
  sleep 1
  COUNTER=$((COUNTER+1))
  if [ "$COUNTER" = 120 ]
  then
  	cat ffead.log
  	cat logs/jobs.log
    echo "exiting....dlib"
    exit 1
  fi
done
rm -f serv.ctrl
pkill ffead-cpp

#Start postgresql
service postgresql start
#For profiling/benchmarking

sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' resources/server.prop
nohup bash -c "./server.sh > ffead.log &"
sleep 5
echo "ffead-cpp with sql-raw support launched"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 15 -c 256 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/fortunes"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 15 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/db"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 15 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/queries?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 15 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/updates?queries=20"
echo "normal shutdown"
rm -f serv.ctrl
pkill ffead-cpp

cd /

exit 0
