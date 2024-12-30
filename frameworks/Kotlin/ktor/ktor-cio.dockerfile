FROM maven:3.9.7-amazoncorretto-17-debian as maven
WORKDIR /ktor
COPY ktor/pom.xml pom.xml
COPY ktor/src src
RUN mvn clean package -q

FROM amazoncorretto:17.0.11-al2023-headless
WORKDIR /ktor
COPY --from=maven /ktor/target/tech-empower-framework-benchmark-1.0-SNAPSHOT-cio-bundle.jar app.jar

EXPOSE 9090

CMD ["java", "-jar", "app.jar"]
