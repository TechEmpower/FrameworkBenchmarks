FROM maven:3.8.4-openjdk-17-slim as maven
WORKDIR /microhttp
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:17.0.2
WORKDIR /microhttp
COPY --from=maven /microhttp/target/microhttp-example-0.1-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-cp", "app.jar", "db.DbWebServer"]
