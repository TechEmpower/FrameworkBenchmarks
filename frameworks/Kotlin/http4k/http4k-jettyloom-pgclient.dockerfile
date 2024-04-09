FROM gradle:8.7.0-jdk21
USER root
WORKDIR /http4k
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY core core
COPY core-pgclient core-pgclient
COPY jettyloom-pgclient jettyloom-pgclient
RUN gradle --quiet --no-daemon jettyloom-pgclient:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "--enable-preview", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "jettyloom-pgclient/build/libs/http4k-benchmark.jar"]
