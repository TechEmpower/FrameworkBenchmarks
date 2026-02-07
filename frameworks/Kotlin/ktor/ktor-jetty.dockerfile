FROM gradle:8.13-jdk21 AS build
WORKDIR /ktor
COPY ktor/ ./
RUN chmod +x gradlew && ./gradlew --no-daemon clean jettyBundle

FROM amazoncorretto:21-al2023-headless
WORKDIR /ktor
COPY --from=build /ktor/build/libs/tech-empower-framework-benchmark-1.0-SNAPSHOT-jetty-bundle.jar app.jar

EXPOSE 9090

CMD ["java", "-server","-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "app.jar"]
