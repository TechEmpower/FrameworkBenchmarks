#!/bin/bash

JVM_OPTS="-server \
  -Xms2G \
  -Xmx2G \
  -XX:+AlwaysPreTouch \
  -XX:+UseParallelGC \
  -XX:+PreserveFramePointer \
  -XX:+EnableDynamicAgentLoading \
  -XX:InitialCodeCacheSize=512m \
  -XX:ReservedCodeCacheSize=512m \
  -XX:MaxInlineLevel=20 \
  -XX:+UseNUMA \
  -Djava.lang.Integer.IntegerCache.high=10000 \
  -Dvertx.disableMetrics=true \
  -Dvertx.disableH2c=true \
  -Dvertx.disableWebsockets=true \
  -Dvertx.flashPolicyHandler=false \
  -Dvertx.threadChecks=false \
  -Dvertx.disableContextTimings=true \
  -Dvertx.disableTCCL=true \
  -Dvertx.disableHttpHeadersValidation=true \
  -Dvertx.eventLoopPoolSize=$((`grep --count ^processor /proc/cpuinfo`)) \
  -Dlog4j2.contextSelector=org.apache.logging.log4j.core.async.AsyncLoggerContextSelector \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Dtfb.hasDB=false"

JAR_PATH="./build/libs/vertx-web-kotlin-dsljson-benchmark-1.0.0-SNAPSHOT-fat.jar"

cleanup() {
    echo "Caught SIGINT signal. Stopping the Java program..."
    if [ ! -z "$JAVA_PID" ]; then
        kill -SIGTERM "$JAVA_PID"
        wait "$JAVA_PID"
    fi
    exit 0
}

trap cleanup SIGINT

java $JVM_OPTS -jar $JAR_PATH &
JAVA_PID=$!

echo "Server PID: $JAVA_PID"

wait "$JAVA_PID"
