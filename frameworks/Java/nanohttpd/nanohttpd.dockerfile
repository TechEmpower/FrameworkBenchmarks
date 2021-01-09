FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /nanohttpd
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /nanohttpd
COPY --from=maven /nanohttpd/target/nanohttpd-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-Xss256k", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]
