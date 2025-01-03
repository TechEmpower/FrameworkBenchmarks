FROM gradle:jdk21 as build
WORKDIR /app
COPY ktor-asyncdb/gradle gradle
COPY ktor-asyncdb/build.gradle.kts build.gradle.kts
COPY ktor-asyncdb/gradlew gradlew
COPY ktor-asyncdb/src src
RUN /app/gradlew --no-daemon shadowJar

FROM amazoncorretto:21-al2023-headless
WORKDIR /app
COPY --from=build /app/build/libs/ktor-asyncdb.jar ktor-asyncdb.jar

EXPOSE 9090

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "ktor-asyncdb.jar", "jasync-sql"]
