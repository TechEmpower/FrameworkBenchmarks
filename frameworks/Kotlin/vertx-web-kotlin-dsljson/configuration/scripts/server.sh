#!/bin/bash

NUM_PROCESSORS=$((`grep --count ^processor /proc/cpuinfo`))

JVM_OPTS="-server \
 -Xms2G \
 -Xmx2G \
 -XX:+UseNUMA \
 -XX:+UseParallelGC \
 -Dvertx.disableMetrics=true \
 -Dvertx.disableH2c=true \
 -Dvertx.disableWebsockets=true \
 -Dvertx.flashPolicyHandler=false \
 -Dvertx.threadChecks=false \
 -Dvertx.disableContextTimings=true \
 -Dvertx.disableTCCL=true \
 -Dvertx.disableHttpHeadersValidation=true \
 -Dlog4j2.contextSelector=org.apache.logging.log4j.core.async.AsyncLoggerContextSelector \
 -Dio.netty.buffer.checkBounds=false \
 -Dio.netty.buffer.checkAccessible=false"

JAR_PATH="./build/libs/vertx-web-kotlin-dsljson-benchmark-1.0.0-SNAPSHOT-fat.jar"

VERTX_ARGS="-instances 1"

cleanup() {
    echo "Caught SIGINT signal. Stopping the Java program..."
    if [ ! -z "$JAVA_PID" ]; then
        kill -SIGTERM "$JAVA_PID"
        wait "$JAVA_PID"
    fi
    exit 0
}

trap cleanup SIGINT

java $JVM_OPTS -jar $JAR_PATH $VERTX_ARGS &
JAVA_PID=$!

echo "Server PID: $JAVA_PID"

wait "$JAVA_PID"
