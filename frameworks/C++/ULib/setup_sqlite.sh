#!/bin/bash

export UMEMPOOL="545,0,0,37,8465,0,-17,-22,34"
export ORM_DRIVER="sqlite"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=${ULIB_ROOT}/db/%.*s"

# 1. Change ULib Server configuration
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" $ULIB_ROOT/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
$ULIB_ROOT/bin/userver_tcp -c $ULIB_ROOT/benchmark.cfg &
