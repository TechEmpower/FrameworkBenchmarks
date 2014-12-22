#!/bin/bash

export USE_MYSQL=1
export MYSQL_USER=benchmarkdbuser
export MYSQL_PASS=benchmarkdbpass
export MYSQL_HOST=$DBHOST
export MYSQL_DB=hello_world

export LWAN_ROOT=${IROOT}/lwan
export LWAN_BUILD=${LWAN_ROOT}/build

rm -rf ${LWAN_BUILD}
mkdir -p ${LWAN_BUILD}
cd ${LWAN_BUILD}
cmake ${LWAN_ROOT} -DCMAKE_BUILD_TYPE=Release && make techempower
cd $LWAN_ROOT/techempower
$LWAN_BUILD/techempower/techempower &
