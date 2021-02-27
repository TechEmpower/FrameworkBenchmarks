FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /dropwizard
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q -P postgres

FROM openjdk:11.0.3-jdk-slim
WORKDIR /dropwizard
COPY --from=maven /dropwizard/target/hello-world-0.0.1-SNAPSHOT.jar app.jar
COPY hello-world-postgres.yml hello-world-postgres.yml

EXPOSE 9090

CMD ["java", "-jar", "app.jar", "server", "hello-world-postgres.yml"]
