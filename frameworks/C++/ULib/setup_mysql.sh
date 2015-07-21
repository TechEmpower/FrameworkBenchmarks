#!/bin/bash

export ULIB_VERSION=1.4.2
export ULIB_ROOT=$IROOT/ULib
export ULIB_DOCUMENT_ROOT=${ULIB_ROOT}/ULIB_DOCUMENT_ROOT

export ORM_DRIVER="mysql"
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=hello_world"

export UMEMPOOL="136,0,0,85,1160,155,-17,-22,40"

# 1. Change ULib Server configuration
sed -i "s|PREFORK_CHILD .*|PREFORK_CHILD ${MAX_THREADS}|g" $IROOT/ULib/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
$IROOT/ULib/bin/userver_tcp -c $IROOT/ULib/benchmark.cfg &
