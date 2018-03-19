#!/bin/bash

fw_depends java sbt

sbt 'oneJar' -batch

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -Dio.netty.recycler.maxCapacityPerThread=0 -Dio.netty.leakDetection.level=disabled -jar target/scala-2.11/*finagle*one-jar.jar &
