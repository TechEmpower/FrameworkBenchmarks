#!/bin/bash
export URWEB_HOME=${IROOT}/urweb

${URWEB_HOME}/bin/urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=${DBHOST}" bench

export LD_LIBRARY_PATH=${URWEB_HOME}/lib

MAX_THREADS=$((2 * $MAX_THREADS))
./bench.exe -q -k -t ${MAX_THREADS} &
