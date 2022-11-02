FROM docker.io/maven:3.8.6-eclipse-temurin-19 as maven
WORKDIR /helidon
COPY nima/src src
COPY nima/pom.xml pom.xml
RUN mvn package -q

FROM openjdk:19-jdk-slim
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark-nima.jar app.jar

EXPOSE 8080

CMD java --enable-preview \
    -jar app.jar
