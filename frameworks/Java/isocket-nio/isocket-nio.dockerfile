FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /isocket
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:openjdk:8-alpine
WORKDIR /isocket
COPY --from=maven /isocket/target/isocket-nio-benchmark-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "org.smartboot.http.Bootstrap"]
