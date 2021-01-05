FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /ktor
COPY ktor/pom.xml pom.xml
COPY ktor/src src
RUN mvn clean package -q

FROM openjdk:11.0.3-jdk-stretch
WORKDIR /ktor
COPY --from=maven /ktor/target/tech-empower-framework-benchmark-1.0-SNAPSHOT-jetty-bundle.jar app.jar

EXPOSE 9090

CMD ["java", "-jar", "app.jar"]
