FROM maven:3.9.9-eclipse-temurin-24-noble as maven
WORKDIR /jooby
COPY pom.xml pom.xml
COPY src src
COPY public public
COPY conf conf
RUN mvn package -q -P netty

EXPOSE 8080

CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:+UseNUMA", "-XX:+UseParallelGC", "--enable-native-access=ALL-UNNAMED", "--add-opens=java.base/java.lang=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow", "-Dio.netty.disableHttpHeadersValidation=true", "-Dio.netty.buffer.checkBounds=false", "-Dio.netty.buffer.checkAccessible=false", "-Dio.netty.noUnsafe=false", "-Dio.netty.eventLoopGroup=single", "-cp", "target/jooby.jar", "com.techempower.ReactivePg"]
