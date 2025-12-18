FROM gradle:9.2.1-jdk25

WORKDIR /vertx-web-kotlinx-exposed-vertx-sql-client

# copy the Maven local dependencies into the container for snapshot dependencies
# First publish with `publishToMavenLocal` and copy the Maven local dependencies into this directory with `cp -r ~/.m2 ./`.
COPY .m2/repository/com/huanshankeji/exposed-vertx-sql-client-core/0.7.0-SNAPSHOT /root/.m2/repository/com/huanshankeji/exposed-vertx-sql-client-core/0.7.0-SNAPSHOT
COPY .m2/repository/com/huanshankeji/exposed-vertx-sql-client-postgresql/0.7.0-SNAPSHOT /root/.m2/repository/com/huanshankeji/exposed-vertx-sql-client-postgresql/0.7.0-SNAPSHOT


COPY gradle/libs.versions.toml gradle/libs.versions.toml
COPY buildSrc buildSrc
COPY settings.gradle.kts settings.gradle.kts
COPY build.gradle.kts build.gradle.kts
COPY gradle.properties gradle.properties

COPY common/build.gradle.kts common/build.gradle.kts
COPY common/src common/src

COPY with-db/common/build.gradle.kts with-db/common/build.gradle.kts
COPY with-db/common/src with-db/common/src

COPY with-db/exposed-vertx-sql-client/build.gradle.kts with-db/exposed-vertx-sql-client/build.gradle.kts
COPY with-db/exposed-vertx-sql-client/src with-db/exposed-vertx-sql-client/src


RUN gradle --no-daemon with-db:exposed-vertx-sql-client:installDist

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
    with-db/exposed-vertx-sql-client/build/install/exposed-vertx-sql-client/bin/exposed-vertx-sql-client
