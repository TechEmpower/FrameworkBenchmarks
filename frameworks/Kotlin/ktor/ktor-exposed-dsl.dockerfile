FROM gradle:jdk17

WORKDIR /ktor-exposed
COPY ktor-exposed/settings.gradle.kts settings.gradle.kts
COPY ktor-exposed/app app
RUN gradle shadowJar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "app/build/libs/app-all.jar", "Dsl"]
