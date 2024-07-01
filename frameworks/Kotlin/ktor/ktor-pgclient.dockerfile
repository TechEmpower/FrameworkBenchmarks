FROM maven:3.9.7-amazoncorretto-17-debian as build
WORKDIR /app
COPY ktor-pgclient/gradle gradle
COPY ktor-pgclient/build.gradle.kts build.gradle.kts
COPY ktor-pgclient/gradlew gradlew
COPY ktor-pgclient/src src
RUN /app/gradlew --no-daemon shadowJar

FROM amazoncorretto:17.0.11-al2023-headless
WORKDIR /app
COPY --from=build /app/build/libs/ktor-pgclient.jar ktor-pgclient.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:MaxRAMFraction=1", "-XX:-UseBiasedLocking", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "ktor-pgclient.jar"]
