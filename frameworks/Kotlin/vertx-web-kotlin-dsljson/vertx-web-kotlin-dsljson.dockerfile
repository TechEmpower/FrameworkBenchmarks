# --- Build stage with JDK 25 ---
FROM gradle:9.2-jdk25-corretto AS builder

WORKDIR /vertx-web-kotlin-dsljson

COPY src src
COPY buildSrc buildSrc
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY gradle.properties gradle.properties
COPY gradle/libs.versions.toml gradle/libs.versions.toml

RUN gradle shadowJar --no-daemon

# --- Runtime stage using Amazon Corretto 25 ---
FROM amazoncorretto:25

WORKDIR /app

COPY --from=builder \
  /vertx-web-kotlin-dsljson/build/libs/vertx-web-kotlin-dsljson-benchmark-1.0.0-SNAPSHOT-fat.jar \
  vertx-web-kotlin-dsljson.jar

EXPOSE 8080

CMD java \
  -server \
  --enable-native-access=ALL-UNNAMED \
  --sun-misc-unsafe-memory-access=allow \
  --add-opens=java.base/java.nio=ALL-UNNAMED \
  --add-opens=java.base/sun.nio.ch=ALL-UNNAMED \
  --add-opens=java.base/jdk.internal.misc=ALL-UNNAMED \
  --add-opens=java.base/java.lang=ALL-UNNAMED \
  -Xms2G \
  -Xmx2G \
  -XX:MaxDirectMemorySize=6G \
  -XX:+AlwaysPreTouch \
  -XX:+UseParallelGC \
  -XX:+DisableExplicitGC \
  -XX:InitialCodeCacheSize=512m \
  -XX:ReservedCodeCacheSize=512m \
  -XX:+UseNUMA \
  -XX:AutoBoxCacheMax=20000 \
  -XX:+UnlockExperimentalVMOptions \
  -XX:+UseCompactObjectHeaders \
  -Djava.net.preferIPv4Stack=true \
  -Dvertx.disableMetrics=true \
  -Dvertx.disableWebsockets=true \
  -Dvertx.disableContextTimings=true \
  -Dvertx.cacheImmutableHttpResponseHeaders=true \
  -Dvertx.internCommonHttpRequestHeadersToLowerCase=true \
  -Dvertx.disableHttpHeadersValidation=true \
  -Dio.netty.noUnsafe=false \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Dio.netty.leakDetection.level=disabled \
  -Dio.netty.tryReflectionSetAccessible=true \
  -Dio.netty.iouring.ringSize=16384 \
  -Dio.netty.iouring.cqSize=32768 \
  -Dtfb.type=basic \
  -jar /app/vertx-web-kotlin-dsljson.jar
