#!/bin/bash

fw_depends mysql racket

cd bench
racket -t bench.rkt -- ${DBHOST} &
