#!/bin/bash

cd bench
$RACKET_HOME/bin/racket -t bench.rkt -- ${DBHOST} &