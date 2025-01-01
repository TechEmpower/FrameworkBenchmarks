FROM maven:3.9.9-amazoncorretto-21-debian-bookworm as maven
WORKDIR /app
COPY ktor-asyncdb/gradle gradle
COPY ktor-asyncdb/build.gradle.kts build.gradle.kts
COPY ktor-asyncdb/gradle.properties gradle.properties
COPY ktor-asyncdb/gradlew gradlew
COPY ktor-asyncdb/settings.gradle settings.gradle
COPY ktor-asyncdb/src src
RUN /app/gradlew --no-daemon shadowJar

EXPOSE 9090

CMD ["java", "-server","-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "/app/build/libs/bench.jar", "jasync-sql"]
