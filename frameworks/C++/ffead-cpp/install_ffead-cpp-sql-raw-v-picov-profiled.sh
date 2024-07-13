#!/bin/bash

export FFEAD_CPP_PATH=${IROOT}/ffead-cpp-7.0-sql
export LD_LIBRARY_PATH=${IROOT}/:${IROOT}/lib:${FFEAD_CPP_PATH}/lib:/usr/local/lib:$LD_LIBRARY_PATH

cd $IROOT/lang-server-backends/v/pico.v

APP=t3
if [ "$1" = "async" ]
then
	APP=t4
	cp -f ${FFEAD_CPP_PATH}/web/t4/config/cachememory.xml ${FFEAD_CPP_PATH}/web/t4/config/cache.xml
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
	nohup bash -c "./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 --is_async=true > ffead.log &"
elif [ "$1" = "async-pool" ]
then
	APP=t4
	cp -f ${FFEAD_CPP_PATH}/web/t4/config/cachememory.xml ${FFEAD_CPP_PATH}/web/t4/config/cache.xml
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
	sed -i 's|"TeBkUmLpqAsyncRouter"|"TeBkUmLpqAsyncRouterPooled"|g' ${FFEAD_CPP_PATH}/web/t4/config/application.xml
	sed -i 's|TeBkUmLpqAsyncRouter|TeBkUmLpqAsyncRouterPooled|g' ${FFEAD_CPP_PATH}/web/t4/config/cachememory.xml
	nohup bash -c "./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 --is_async=true > ffead.log &"
if [ "$1" = "wire" ]
then
	APP=t6
	cp -f ${FFEAD_CPP_PATH}/web/t6/config/cachememory.xml ${FFEAD_CPP_PATH}/web/t6/config/cache.xml
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
	nohup bash -c "./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 --is_async=true > ffead.log &"
elif [ "$1" = "async-wire" ]
then
	APP=t7
	cp -f ${FFEAD_CPP_PATH}/web/t7/config/cachememory.xml ${FFEAD_CPP_PATH}/web/t7/config/cache.xml
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
	nohup bash -c "./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 --is_async=true > ffead.log &"
else
	cp -f ${FFEAD_CPP_PATH}/web/t3/config/cachememory.xml ${FFEAD_CPP_PATH}/web/t3/config/cache.xml
	sed -i 's|"TeBkUmLpqRouter"|"TeBkUmLpqRouterPicoV"|g' ${FFEAD_CPP_PATH}/web/t3/config/application.xml
	sed -i 's|EVH_SINGLE=false|EVH_SINGLE=true|g' ${FFEAD_CPP_PATH}/resources/server.prop
	nohup bash -c "./main --server_dir=$FFEAD_CPP_PATH --server_port=8080 --is_async=false > ffead.log &"
fi


sleep 30
echo "ffead-cpp-v-picov with sql-raw support launched"
if [ "$1" = "async" ] || [ "$1" = "async-pool" ]
then
	wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/$APP/plaint"
else
	wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/plaintext"
fi

wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/$APP/j"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 256 --timeout 8 -t 2 "http://localhost:8080/$APP/fortu"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/d"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/quer?queries=20"
if [ "$1" = "async" ] || [ "$1" = "async-pool" ]
then
	wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
		-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/quem?queries=20"
else
	wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
		-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/que_?queries=20"
fi
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/updt?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/upd_?queries=20"
wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
	-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/cached-wld?count=20"
if [ "$1" = "async" ] || [ "$1" = "async-pool" ]
then
	wrk -H 'Host: localhost' -H 'Accept: application/json,text/html;q=0.9,application/xhtml+xml;q=0.9,application/xml;q=0.8,*/*;q=0.7' \
		-H 'Connection: keep-alive' --latency -d 5 -c 512 --timeout 8 -t 2 "http://localhost:8080/$APP/updm?queries=20"
fi
echo "normal shutdown"
pkill main

cd /

exit 0
