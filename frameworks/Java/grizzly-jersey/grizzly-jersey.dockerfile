FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /grizzly-jersey
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /grizzly-jersey
COPY --from=maven /grizzly-jersey/target/grizzly-jersey-example.jar app.jar
CMD ["java", "-jar", "app.jar"]
