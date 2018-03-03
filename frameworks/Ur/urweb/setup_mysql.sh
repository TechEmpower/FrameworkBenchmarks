#!/bin/bash

fw_depends urweb mysql

export URWEB_HOME=${IROOT}/urweb
export LD_LIBRARY_PATH=${URWEB_HOME}/lib

${URWEB_HOME}/bin/urweb -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=${DBHOST}" bench

MAX_THREADS=$((2 * $CPU_COUNT))
./bench.exe -q -k -t ${MAX_THREADS} &
