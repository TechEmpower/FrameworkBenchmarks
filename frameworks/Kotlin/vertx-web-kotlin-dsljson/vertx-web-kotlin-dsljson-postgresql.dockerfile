FROM gradle:8.9-jdk21 as gradle

WORKDIR /vertx-web-kotlin-dsljson

COPY src src
COPY build.gradle.kts build.gradle.kts
COPY gradle.properties gradle.properties
COPY settings.gradle.kts settings.gradle.kts

RUN gradle shadowJar

EXPOSE 8080

CMD java \
    -server \
    -Xms2G \
    -Xmx2G \
    -XX:+AlwaysPreTouch \
    -XX:+UseParallelGC \
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
    -Dtfb.hasDB=true \
    -jar \
    build/libs/vertx-web-kotlin-dsljson-benchmark-1.0.0-SNAPSHOT-fat.jar
