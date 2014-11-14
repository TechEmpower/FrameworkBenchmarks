#!/bin/bash

export UMEMPOOL="135,0,0,34,8465,129,-17,-22,41"
export ORM_DRIVER="mysql"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=hello_world"

# 1. Change ULib Server configuration
sed -i 's|PREFORK_CHILD .*|PREFORK_CHILD '"${MAX_THREADS}"'|g' $ULIB_ROOT/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
$ULIB_ROOT/bin/userver_tcp -c $ULIB_ROOT/benchmark.cfg