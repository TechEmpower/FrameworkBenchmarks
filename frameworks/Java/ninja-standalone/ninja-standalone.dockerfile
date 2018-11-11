FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /ninja-standalone
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /ninja-standalone
COPY --from=maven /ninja-standalone/target/ninja-standalone-0.0.1-SNAPSHOT-jar-with-dependencies.jar app.jar
CMD ["java", "-Dninja.port=8080", "-jar", "app.jar"]
