#!/bin/bash

# JFR: -XX:+FlightRecorder -XX:StartFlightRecording=duration=60s,filename=/quarkus/trace.jfr
#       and use docker cp to read it

# PROFILING: -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints

# DEBUG: -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=*:5005

# Consider using -Dquarkus.http.io-threads=$((`grep --count ^processor /proc/cpuinfo`)) \

JAVA_OPTIONS="-server \
  -Dquarkus.vertx.prefer-native-transport=true  \
  -XX:-StackTraceInThrowable \
  -Dquarkus.http.accept-backlog=-1 \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Djava.util.logging.manager=org.jboss.logmanager.LogManager \
  -Dquarkus.http.idle-timeout=0 \
  -XX:-UseBiasedLocking \
  -XX:+UseStringDeduplication \
  -XX:+UseNUMA \
  -XX:+UseParallelGC \
  -Djava.lang.Integer.IntegerCache.high=10000 \
  -Dvertx.disableURIValidation=true \
  -Dvertx.disableHttpHeadersValidation=true \
  -Dvertx.disableMetrics=true \
  -Dvertx.disableH2c=true \
  -Dvertx.disableWebsockets=true \
  -Dvertx.flashPolicyHandler=false \
  -Dvertx.threadChecks=false \
  -Dvertx.disableContextTimings=true \
  -Dhibernate.allow_update_outside_transaction=true \
  -Dio.quarkus.vertx.core.runtime.context.VertxContextSafetyToggle.I_HAVE_CHECKED_EVERYTHING=true \
  -Djboss.threads.eqe.statistics=false \
  -Dmutiny.disableCallBackDecorators=true \
  $@"

java $JAVA_OPTIONS -jar quarkus-run.jar
