#!/bin/bash

${URWEB_HOME}/bin/urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=${DBHOST}" bench

./bench.exe -q -k -t ${MAX_THREADS} &