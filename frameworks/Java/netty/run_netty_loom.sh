#!/bin/bash

# PROFILING: -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints
JAVA_OPTIONS="--enable-native-access=ALL-UNNAMED \
  -Dio.netty.noUnsafe=false \
  --sun-misc-unsafe-memory-access=allow \
  --add-opens=java.base/java.lang=ALL-UNNAMED \
  -XX:+UseNUMA \
  -XX:+UseParallelGC \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Dhello.eventloop.carrier=true \
  -XX:+UnlockExperimentalVMOptions \
  -XX:-DoJVMTIVirtualThreadTransitions \
  -Djdk.trackAllThreads=false \
  $@"

java $JAVA_OPTIONS -jar app.jar
