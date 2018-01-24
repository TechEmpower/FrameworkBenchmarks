#!/bin/bash

fw_depends java sbt

sbt clean assembly -batch

java -Dcom.twitt.finagle.netty4.numWorkers=1 -Dcom.twitter.util.events.sinkEnabled=false -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar target/scala-2.12/finatra-benchmark.jar -log.level=ERROR -http.response.charset.enabled=false
