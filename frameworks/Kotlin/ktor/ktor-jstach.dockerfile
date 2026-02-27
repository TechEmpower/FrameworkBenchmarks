FROM gradle:8.13-jdk21 AS build
WORKDIR /ktor-jstach
COPY ktor-jstach/ ./
RUN chmod +x gradlew && ./gradlew --no-daemon clean nettyBundle

FROM amazoncorretto:21-al2023-headless
WORKDIR /ktor-jstach
COPY --from=build /ktor-jstach/build/libs/tech-empower-framework-benchmark-1.0-SNAPSHOT-netty-bundle.jar app.jar

EXPOSE 9090

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-Djava.lang.Integer.IntegerCache.high=10000", "-jar", "app.jar"]
