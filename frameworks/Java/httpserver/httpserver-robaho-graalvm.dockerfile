FROM maven:3-eclipse-temurin-25-alpine as maven
WORKDIR /httpserver-robaho
COPY pom.xml pom.xml
COPY src src
RUN mvn compile -P robaho assembly:single -q

FROM ghcr.io/graalvm/graalvm-community:25
WORKDIR /httpserver-robaho
COPY --from=maven /httpserver-robaho/target/httpserver-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-jar", "app.jar"]
