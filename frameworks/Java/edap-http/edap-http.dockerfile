FROM maven:3.6.3-openjdk-8-slim as maven
WORKDIR /edap-http
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:8u275-jdk-slim
WORKDIR /edap-http
COPY --from=maven /edap-http/target/edap-http-benchmark-1.0-SNAPSHOT-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "io.edap.http.Bootstrap"]
