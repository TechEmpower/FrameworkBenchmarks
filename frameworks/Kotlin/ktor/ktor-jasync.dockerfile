FROM gradle:jdk21

WORKDIR /ktor-asyncdb
COPY ktor-asyncdb/settings.gradle settings.gradle
COPY ktor-asyncdb/app app
RUN gradle --no-daemon shadowJar

EXPOSE 9090

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "/app/build/libs/bench.jar", "jasync-sql"]
