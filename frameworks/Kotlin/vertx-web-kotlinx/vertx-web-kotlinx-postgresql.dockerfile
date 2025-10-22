FROM gradle:8.10.2-jdk21

WORKDIR /vertx-web-kotlinx
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY gradle.properties gradle.properties
COPY src src
RUN gradle --no-daemon installDist

EXPOSE 8080

CMD export JAVA_OPTS=" \
    -server \
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
    -Dio.netty.buffer.checkBounds=false \
    -Dio.netty.buffer.checkAccessible=false \
    " && \
    build/install/vertx-web-kotlinx-benchmark/bin/vertx-web-kotlinx-benchmark true
