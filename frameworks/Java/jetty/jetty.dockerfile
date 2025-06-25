FROM maven:3-eclipse-temurin-24-alpine as maven
WORKDIR /jetty
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:25-jdk-slim
WORKDIR /jetty
COPY --from=maven /jetty/target/jetty-example-0.1-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
