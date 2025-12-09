FROM gradle:9.2.1-jdk25

WORKDIR /vertx-web-kotlinx
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY gradle.properties gradle.properties
COPY src src
RUN gradle --no-daemon installDist

EXPOSE 8080

CMD export JAVA_OPTS=" \
    --enable-native-access=ALL-UNNAMED \
    --sun-misc-unsafe-memory-access=allow \
    --add-opens=java.base/java.lang=ALL-UNNAMED \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+UnlockDiagnosticVMOptions \
    -XX:+DebugNonSafepoints \
    -Djava.lang.Integer.IntegerCache.high=10000 \
    -Dvertx.disableMetrics=true \
    -Dvertx.disableWebsockets=true \
    -Dvertx.disableContextTimings=true \
    -Dvertx.disableHttpHeadersValidation=true \
    -Dvertx.cacheImmutableHttpResponseHeaders=true \
    -Dvertx.internCommonHttpRequestHeadersToLowerCase=true \
    -Dio.netty.noUnsafe=false \
    -Dio.netty.buffer.checkBounds=false \
    -Dio.netty.buffer.checkAccessible=false \
    " && \
    build/install/vertx-web-kotlinx-benchmark/bin/vertx-web-kotlinx-benchmark false
