#!/bin/bash

fw_depends mysql java sbt

sbt 'assembly' -batch

java -server -Dakka.http.benchmark.mysql.dbhost=$DBHOST -jar target/scala-2.12/akka-http-benchmark.jar &
