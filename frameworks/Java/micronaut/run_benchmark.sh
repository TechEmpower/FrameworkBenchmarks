#!/bin/bash

if [ -z "$MN_ENV" ]; then
  MN_ENV=benchmark
else
  MN_ENV=benchmark,$MN_ENV
fi

JAVA_OPTIONS="$JAVA_OPTIONS -server \
  -XX:+UseParallelGC \
  -XX:+UseNUMA \
  -Djdk.trackAllThreads=false \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Dvertx.disableMetrics=true \
  -Dvertx.threadChecks=false \
  -Dvertx.disableContextTimings=true  \
  -Dvertx.disableTCCL=true  \
  -Dmicronaut.environments=$MN_ENV \
  --add-opens=java.base/java.lang=ALL-UNNAMED \
  $@"

exec java $JAVA_OPTIONS -jar micronaut.jar
