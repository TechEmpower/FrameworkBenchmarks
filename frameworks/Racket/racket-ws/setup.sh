#!/bin/bash

fw_depends racket

cd bench
racket -t bench.rkt -- ${DBHOST} &
