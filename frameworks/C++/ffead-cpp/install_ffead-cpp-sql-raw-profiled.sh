#!/bin/bash

chmod +x $IROOT/ffead-cpp-sql-raw/*.sh

SUFFIX=""
if [ "$1" = "async" ]
then
	SUFFIX="-async"
fi

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
    echo "ffead-cpp exiting exiting due to failure...."
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
    echo "ffead-cpp exiting exiting due to failure....dlib"
    exit 1
  fi
done
echo "ffead-cpp start successful"
sleep 5
#cd tests && chmod +x *.sh && ./runTests.sh
#echo "ffead-cpp normal shutdown"
#cd -
rm -f serv.ctrl
pkill ffead-cpp

#Start postgresql
service postgresql start
#For profiling/benchmarking

sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' resources/server.prop
#sed -i 's|LOGGING_ENABLED=false|LOGGING_ENABLED=true|g' resources/server.prop
nohup bash -c "./server.sh > ffead.log &"
sleep 10
echo "ffead-cpp with sql-raw support launched"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq${SUFFIX}/fortunes"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq${SUFFIX}/db"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq${SUFFIX}/queries?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq${SUFFIX}/updates?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq${SUFFIX}/bupdates?queries=20"
echo "normal shutdown"
rm -f serv.ctrl
pkill ffead-cpp

cd /

exit 0
