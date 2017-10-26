#!/bin/bash

fw_depends mysql java sbt

sbt -batch 'universal:stage'

./target/universal/stage/bin/akka-http-benchmark \
  -Dakka.http.benchmark.mysql.dbhost=$DBHOST &
