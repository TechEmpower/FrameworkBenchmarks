#!/bin/bash

java \
  -Xms2G \
  -Xmx2G \
  -server \
  -XX:+UseNUMA \
  -XX:+UseParallelGC \
  -XX:+AggressiveOpts \
  -Dvertx.disableMetrics=true \
  -Dvertx.disableH2c=true \
  -Dvertx.disableWebsockets=true \
  -Dvertx.flashPolicyHandler=false \
  -Dvertx.threadChecks=false \
  -Dvertx.disableContextTimings=true \
  -Dvertx.disableTCCL=true \
  -jar target/vertx.benchmark-0.0.1-SNAPSHOT-fat.jar \
  src/main/conf/config.json
