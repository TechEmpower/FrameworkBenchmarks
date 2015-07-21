#!/bin/bash

export URWEB_HOME=${IROOT}/urweb
export LD_LIBRARY_PATH=${URWEB_HOME}/lib

${URWEB_HOME}/bin/urweb -dbms mysql -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=${DBHOST}" bench

MAX_THREADS=$((2 * $MAX_THREADS))
./bench.exe -q -k -t ${MAX_THREADS} &
