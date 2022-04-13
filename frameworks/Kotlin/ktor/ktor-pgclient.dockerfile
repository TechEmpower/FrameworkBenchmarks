FROM openjdk:11.0.3-jdk-stretch as build
WORKDIR /app
COPY ktor-pgclient/gradle gradle
COPY ktor-pgclient/build.gradle.kts build.gradle.kts
COPY ktor-pgclient/gradlew gradlew
COPY ktor-pgclient/src src
RUN /app/gradlew --no-daemon shadowJar

FROM openjdk:11.0.3-jdk-slim
WORKDIR /app
COPY --from=build /app/build/libs/ktor-pgclient.jar ktor-pgclient.jar

EXPOSE 8080

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:-UseBiasedLocking", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "ktor-pgclient.jar"]
