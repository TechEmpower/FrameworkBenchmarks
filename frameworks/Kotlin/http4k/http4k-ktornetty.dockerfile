FROM gradle:7.5.1-jdk11
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY ktornetty ktornetty
RUN gradle --quiet ktornetty:shadowJar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "ktornetty/build/libs/http4k-ktornetty-benchmark.jar"]
