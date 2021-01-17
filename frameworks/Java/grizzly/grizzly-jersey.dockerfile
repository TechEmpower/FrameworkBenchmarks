FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /grizzly-jersey
COPY pom-jersey.xml pom.xml
COPY src-jersey src
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /grizzly-jersey
COPY --from=maven /grizzly-jersey/target/grizzly-jersey-example.jar app.jar

EXPOSE 8080

CMD ["java", "-jar", "app.jar"]
