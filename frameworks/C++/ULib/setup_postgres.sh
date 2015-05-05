#!/bin/bash

# 1. Change ULib Server (userver_tcp) configuration
sed -i "s|LISTEN_BACKLOG .*|LISTEN_BACKLOG 256|g"			  $IROOT/ULib/benchmark.cfg
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
export ORM_DRIVER="pgsql"
export UMEMPOOL="136,0,0,100,150,-22,-17,-23,40"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world client_encoding=UTF8"

$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
