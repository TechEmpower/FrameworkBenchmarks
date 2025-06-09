FROM maven:3-eclipse-temurin-24-alpine as maven
WORKDIR /ring-http-exchange
COPY pom.xml pom.xml
COPY src src
RUN mvn clean clojure:compile package

FROM openjdk:25-jdk-slim
WORKDIR /ring-http-exchange
COPY --from=maven /ring-http-exchange/target/ring-http-server-1.0.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseParallelGC", "-jar", "app.jar"]
