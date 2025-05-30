FROM maven:3.9.9-eclipse-temurin-24-noble as maven
WORKDIR /vertx
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

EXPOSE 8080

CMD ["java", "--enable-native-access=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow", "--add-opens=java.base/java.lang=ALL-UNNAMED", "-Xms2G", "-Xmx2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Djava.lang.Integer.IntegerCache.high=10000", "-Dvertx.disableMetrics=true", "-Dvertx.disableWebsockets=true", "-Dvertx.disableContextTimings=true", "-Dvertx.disableHttpHeadersValidation=true", "-Dvertx.cacheImmutableHttpResponseHeaders=true", "-Dvertx.internCommonHttpRequestHeadersToLowerCase=true", "-Dio.netty.noUnsafe=false", "-Dio.netty.buffer.checkBounds=false", "-Dio.netty.buffer.checkAccessible=false", "-jar", "target/vertx.benchmark-0.0.1-SNAPSHOT-fat.jar", "src/main/conf/config.json"]
