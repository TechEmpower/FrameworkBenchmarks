FROM gradle:9.2.1-jdk25

WORKDIR /vertx-web-kotlinx


COPY gradle/libs.versions.toml gradle/libs.versions.toml
COPY buildSrc buildSrc
COPY settings.gradle.kts settings.gradle.kts
COPY build.gradle.kts build.gradle.kts
COPY gradle.properties gradle.properties

# make empty directories for subprojects that do not need to be copied for Gradle
RUN mkdir -p common without-db/default with-db/common with-db/default with-db/r2dbc-common with-db/r2dbc with-db/exposed-common with-db/exposed-r2dbc with-db/exposed-vertx-sql-client

COPY common/build.gradle.kts common/build.gradle.kts
COPY common/src common/src

COPY with-db/common/build.gradle.kts with-db/common/build.gradle.kts
COPY with-db/common/src with-db/common/src

COPY with-db/r2dbc-common/build.gradle.kts with-db/r2dbc-common/build.gradle.kts
COPY with-db/r2dbc-common/src with-db/r2dbc-common/src

COPY with-db/exposed-common/build.gradle.kts with-db/exposed-common/build.gradle.kts
COPY with-db/exposed-common/src with-db/exposed-common/src

COPY with-db/exposed-r2dbc/build.gradle.kts with-db/exposed-r2dbc/build.gradle.kts
COPY with-db/exposed-r2dbc/src with-db/exposed-r2dbc/src


RUN gradle --no-daemon with-db:exposed-r2dbc:installDist

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
    -Dio.netty.iouring.ringSize=16384 \
    " && \
    with-db/exposed-r2dbc/build/install/exposed-r2dbc/bin/exposed-r2dbc false 8 true
