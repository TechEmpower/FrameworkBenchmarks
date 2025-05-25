FROM gradle:jdk21

WORKDIR /ktor-exposed
COPY ktor-exposed/settings.gradle.kts settings.gradle.kts
COPY ktor-exposed/app app
RUN gradle --no-daemon shadowJar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseG1GC", "-XX:+AlwaysPreTouch", "-XX:+UseStringDeduplication", "-jar", "app/build/libs/app-all.jar", "Dsl"]
