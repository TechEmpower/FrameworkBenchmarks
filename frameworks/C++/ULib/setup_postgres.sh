#!/bin/bash

export UMEMPOOL="152,85,0,37,8330,105,-17,-22,40"
export ORM_DRIVER="pgsql"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world client_encoding=UTF8"

# 1. Change ULib Server configuration
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
