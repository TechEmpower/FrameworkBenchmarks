FROM maven:3.9.9-amazoncorretto-21-debian-bookworm as maven
WORKDIR /ktor-r2dbc
COPY ktor-r2dbc/pom.xml pom.xml
COPY ktor-r2dbc/src src
RUN mvn clean package -q

FROM amazoncorretto:21-al2023-headless
WORKDIR /ktor-r2dbc
COPY --from=maven /ktor-r2dbc/target/tech-empower-framework-benchmark-1.0-SNAPSHOT-netty-bundle.jar app.jar

EXPOSE 9090

CMD ["java", "-server","-XX:+UseNUMA", "-XX:+UseG1GC", "-XX:+AlwaysPreTouch", "-XX:+UseStringDeduplication", "-jar", "app.jar"]
