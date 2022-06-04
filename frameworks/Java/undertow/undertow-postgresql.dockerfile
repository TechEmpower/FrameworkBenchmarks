FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /undertow
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /undertow
COPY --from=maven /undertow/target/app.jar app.jar

EXPOSE 8080

CMD ["java", "-jar", "app.jar", "POSTGRESQL"]
