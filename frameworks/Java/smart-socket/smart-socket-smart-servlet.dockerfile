FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /smart-socket
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /smart-socket
COPY --from=maven /smart-socket/target/smart-socket-benchmark-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "org.smartboot.servlet.Bootstrap"]
