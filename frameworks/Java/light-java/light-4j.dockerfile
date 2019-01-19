FROM maven:3.6.0-jdk-11-slim as maven
WORKDIR /light-4j
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:11-jdk-slim
WORKDIR /light-4j
COPY --from=maven /light-4j/target/techempower-1.0.0.jar app.jar
CMD ["java", "-jar", "app.jar"]
