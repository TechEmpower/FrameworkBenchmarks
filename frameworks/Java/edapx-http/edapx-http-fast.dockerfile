FROM maven:3.6.3-openjdk-8-slim as maven
WORKDIR /edapx-http
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:8u275-jdk-slim
WORKDIR /edapx-http
COPY --from=maven /edapx-http/target/edap-http-benchmark-1.0-SNAPSHOT-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dedap.http.decoder.type=fast", "-cp", "app.jar", "io.edapx.http.Bootstrap"]
