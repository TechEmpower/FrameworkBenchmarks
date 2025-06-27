FROM maven:3-eclipse-temurin-24-alpine as maven
WORKDIR /httpserver
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM ghcr.io/graalvm/graalvm-community:24
WORKDIR /httpserver
COPY --from=maven /httpserver/target/httpserver-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
