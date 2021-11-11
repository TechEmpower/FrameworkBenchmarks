FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /jetty
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q -P servlet

FROM openjdk:11.0.3-jdk-slim
WORKDIR /jetty
COPY --from=maven /jetty/target/jetty-example-0.1-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
