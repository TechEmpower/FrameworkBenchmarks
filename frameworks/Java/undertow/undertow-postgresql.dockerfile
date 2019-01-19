FROM maven:3.6.0-jdk-11-slim as maven
WORKDIR /undertow
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:11-jdk-slim
WORKDIR /undertow
COPY --from=maven /undertow/target/app.jar app.jar
CMD ["java", "-jar", "app.jar", "POSTGRESQL"]
