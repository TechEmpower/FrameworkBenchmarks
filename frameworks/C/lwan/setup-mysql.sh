#!/bin/bash

export USE_MYSQL=1
export MYSQL_USER=benchmarkdbuser
export MYSQL_PASS=benchmarkdbpass
export MYSQL_HOST=$DBHOST
export MYSQL_DB=hello_world

fw_depends lwan libjemalloc-dev libluajit-5.1-dev

cd $LWAN_ROOT/techempower
$LWAN_BUILD/techempower/techempower &
