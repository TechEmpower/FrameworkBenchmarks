#!/bin/bash

chmod +x $IROOT/ffead-cpp-sql-raw/*.sh

APP_CTXT="t3"
if [ "$1" = "async" ]
then
	APP_CTXT="t4"
elif [ "$1" = "async-qw" ]
then
	APP_CTXT="t5"
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

if [ "$1" = "async" ]
then
	sed -i 's|REQUEST_HANDLER=RequestReaderHandler|REQUEST_HANDLER=RequestHandler2|g' resources/server.prop
fi

if [ "$1" = "async-qw" ]
then
	sed -i 's|REQUEST_HANDLER=RequestReaderHandler|REQUEST_HANDLER=RequestHandler2|g' resources/server.prop
	sed -i 's|QUEUED_WRITES=false|QUEUED_WRITES=true|g' resources/server.prop
fi

nohup bash -c "./server.sh > ffead.log &"
sleep 10
echo "ffead-cpp with sql-raw support launched"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/plaintext"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/j"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/fortu"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/d"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/quer?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/quem?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/que_?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/updt?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/updm?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/${APP_CTXT}/upd_?queries=20"

echo "normal shutdown"
rm -f serv.ctrl
pkill ffead-cpp

cd /

exit 0
