#!/bin/bash

# JFR: -XX:+FlightRecorder -XX:StartFlightRecording=duration=60s,filename=/quarkus/trace.jfr
#       and use docker cp to read it

# PROFILING: -XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints

# DEBUG: -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=*:5005

# Consider using -Dquarkus.http.io-threads=$((`grep --count ^processor /proc/cpuinfo`)) \

JAVA_OPTIONS_AUTOTUNE="-server -XX:MaxRAMPercentage=70 -Dquarkus.thread-pool.core-threads=1 -Dquarkus.thread-pool.queue-size=7590 -Dquarkus.datasource.jdbc.min-size=8 -Dquarkus.datasource.jdbc.initial-size=8 -Dquarkus.datasource.jdbc.max-size=39 -Dquarkus.http.io-threads=4 -XX:FreqInlineSize=405 -XX:MaxInlineLevel=27 -XX:MinInliningThreshold=38 -XX:CompileThreshold=8740 -XX:CompileThresholdScaling=1 -XX:ConcGCThreads=1 -XX:InlineSmallCode=3257 -XX:LoopUnrollLimit=58 -XX:LoopUnrollMin=18 -XX:MinSurvivorRatio=7 -XX:NewRatio=1 -XX:TieredStopAtLevel=1 -XX:-TieredCompilation -XX:+AllowParallelDefineClass -XX:+AllowVectorizeOnDemand -XX:-AlwaysCompileLoopMethods -XX:-AlwaysPreTouch -XX:+AlwaysTenure -XX:+BackgroundCompilation -XX:+DoEscapeAnalysis -XX:+UseInlineCaches -XX:+UseLoopPredicate -XX:+UseStringDeduplication -XX:-UseSuperWord -XX:-UseTypeSpeculation -XX:-StackTraceInThrowable -XX:+UseParallelGC -Dio.netty.buffer.checkBounds=false -Dio.netty.buffer.checkAccessible=false"

JAVA_OPTIONS="-server \
  -XX:-UseBiasedLocking \
  -XX:+UseNUMA \
  -Dquarkus.vertx.prefer-native-transport=true  \
  -Dquarkus.http.accept-backlog=-1 \
  -Djava.util.logging.manager=org.jboss.logmanager.LogManager \
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
  ${JAVA_OPTIONS_AUTOTUNE} \
  $@"

java $JAVA_OPTIONS -jar quarkus-run.jar
