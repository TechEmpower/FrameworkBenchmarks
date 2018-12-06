FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /httpserver
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /httpserver
COPY --from=maven /httpserver/target/httpserver-1.0-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-Xss256k", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar", "postgres"]
