FROM gradle:8.13-jdk21 AS build
WORKDIR /ktor-r2dbc
COPY ktor-r2dbc/ ./
RUN chmod +x gradlew && ./gradlew --no-daemon clean nettyBundle

FROM amazoncorretto:21-al2023-headless
WORKDIR /ktor-r2dbc
COPY --from=build /ktor-r2dbc/build/libs/tech-empower-framework-benchmark-1.0-SNAPSHOT-netty-bundle.jar app.jar

EXPOSE 9090

CMD ["java", "-server","-XX:+UseNUMA", "-XX:+UseG1GC", "-XX:+AlwaysPreTouch", "-jar", "app.jar"]
