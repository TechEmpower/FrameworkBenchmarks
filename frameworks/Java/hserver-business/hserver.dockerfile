FROM maven:3.8.4-openjdk-17-slim as maven
WORKDIR /hserver
COPY pom.xml pom.xml
COPY src src
RUN mvn package

FROM openjdk:17.0.2
WORKDIR /hserver
COPY --from=maven /hserver/target/hserver-1.0.jar app.jar

EXPOSE 8888

CMD ["java", "-jar", "app.jar"]
