FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /smart-socket
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:8-jre-slim
WORKDIR /smart-socket
COPY --from=maven /smart-socket/target/smart-socket-benchmark-1.0-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "org.smartboot.http.Bootstrap"]
