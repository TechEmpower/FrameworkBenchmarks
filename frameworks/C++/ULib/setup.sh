#!/bin/bash

export ULIB_VERSION=1.4.2
export ULIB_ROOT=$IROOT/ULib
export ULIB_DOCUMENT_ROOT=${ULIB_ROOT}/ULIB_DOCUMENT_ROOT

export UMEMPOOL="135,0,0,34,8465,129,-17,-22,41"
echo $UMEMPOOL
export ORM_DRIVER="sqlite"
echo $ORM_DRIVER
export ORM_OPTION="host=${DBHOST} user=benchmarkdbuser password=benchmarkdbpass character-set=utf8 dbname=${ULIB_ROOT}/db/%.*s"
echo $ORM_OPTION

# 1. Change ULib Server configuration
sed -i 's|PREFORK_CHILD .*|PREFORK_CHILD '"${MAX_THREADS}"'|g' $ULIB_ROOT/benchmark.cfg

# 2. Start ULib Server (userver_tcp)
$ULIB_ROOT/bin/userver_tcp -c $ULIB_ROOT/benchmark.cfg &