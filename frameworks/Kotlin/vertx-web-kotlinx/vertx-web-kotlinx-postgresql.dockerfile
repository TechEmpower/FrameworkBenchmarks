FROM gradle:7.6-jdk17

WORKDIR /vertx-web-kotlinx
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY gradle.properties gradle.properties
COPY src src
RUN gradle assembleDist
RUN tar -xf build/distributions/vertx-web-kotlinx-benchmark.tar

EXPOSE 8080

# TODO: is running a shadow jar faster?
# TODO: do the extra parameters in the "vertx-web" benchmark improve performance?
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
    vertx-web-kotlinx-benchmark/bin/vertx-web-kotlinx-benchmark
