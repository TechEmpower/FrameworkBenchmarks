#!/bin/bash

JAVA_OPTIONS="-server \
  -XX:+UseParallelGC \
  -XX:+UseNUMA \
  -XX:+UseStringDeduplication \
  -XX:-StackTraceInThrowable \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Dvertx.disableMetrics=true \
  -Dvertx.threadChecks=false \
  -Dvertx.disableContextTimings=true  \
  -Dvertx.disableTCCL=true  \
  -Dmicronaut.environments=benchmark
  $@"

java $JAVA_OPTIONS -jar micronaut.jar
