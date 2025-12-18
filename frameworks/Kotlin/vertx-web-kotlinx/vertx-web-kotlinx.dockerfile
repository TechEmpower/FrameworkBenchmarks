FROM gradle:9.2.1-jdk25

WORKDIR /vertx-web-kotlinx


COPY gradle/libs.versions.toml gradle/libs.versions.toml
COPY buildSrc buildSrc
COPY settings.gradle.kts settings.gradle.kts
COPY build.gradle.kts build.gradle.kts
COPY gradle.properties gradle.properties

# make empty directories for subprojects that do not need to be copied for Gradle
RUN mkdir -p common without-db/default with-db/common with-db/default with-db/r2dbc with-db/exposed-r2dbc with-db/exposed-vertx-sql-client

COPY common/build.gradle.kts common/build.gradle.kts
COPY common/src common/src

COPY without-db/default/build.gradle.kts without-db/default/build.gradle.kts
COPY without-db/default/src without-db/default/src


RUN gradle --no-daemon without-db:default:installDist

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
    without-db/default/build/install/default/bin/default
