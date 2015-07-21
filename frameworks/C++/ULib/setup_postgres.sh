#!/bin/bash

export ORM_DRIVER="pgsql"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world client_encoding=UTF8"

export UMEMPOOL="136,0,0,85,1160,155,-17,-22,40"

# 1. Change ULib Server configuration
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
