FROM gradle:8.7.0-jdk21
USER root
WORKDIR /http4k
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY core core
COPY core-pgclient core-pgclient
COPY jetty11loom-pgclient jetty11loom-pgclient
RUN gradle --quiet --no-daemon jetty11loom-pgclient:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "--enable-preview", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "jetty11loom-pgclient/build/libs/http4k-benchmark.jar"]
