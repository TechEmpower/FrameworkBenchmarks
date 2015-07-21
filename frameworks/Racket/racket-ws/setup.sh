#!/bin/bash
export RACKET_HOME=${IROOT}/racket

cd bench
$RACKET_HOME/bin/racket -t bench.rkt -- ${DBHOST} &