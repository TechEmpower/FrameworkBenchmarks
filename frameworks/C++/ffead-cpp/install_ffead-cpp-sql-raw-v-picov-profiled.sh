#!/bin/bash

export FFEAD_CPP_PATH=${IROOT}/ffead-cpp-6.0-sql
export LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH

cd $IROOT/lang-server-backends/v/pico.v

cp -f ${FFEAD_CPP_PATH}/web/te-benchmark-um-pq/config/cachememory.xml ${FFEAD_CPP_PATH}/web/te-benchmark-um-pq/config/cache.xml
sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
#sed -i 's|LOGGING_ENABLED=false|LOGGING_ENABLED=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
nohup bash -c "./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 > ffead.log &"
sleep 10
echo "ffead-cpp-v-picov with sql-raw support launched"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/plaintext"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/json"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/fortunes"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/db"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/queries?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/updates?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/te-benchmark-um-pq/cached-worlds?count=20"
echo "normal shutdown"
pkill main

cd /

exit 0
