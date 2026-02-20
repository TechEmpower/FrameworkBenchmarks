FROM gradle:jdk25

WORKDIR /ktor-exposed
COPY ktor-exposed/settings.gradle.kts settings.gradle.kts
COPY ktor-exposed/app app
RUN gradle --no-daemon shadowJar

EXPOSE 9090

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-Djava.lang.Integer.IntegerCache.high=10000", "-jar", "app/build/libs/app-all.jar", "Jdbc", "Dao"]
