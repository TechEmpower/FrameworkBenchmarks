FROM gradle:8.7.0-jdk21
USER root
WORKDIR /http4k
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY apache apache
COPY core core
COPY core-jdbc core-jdbc
COPY core-pgclient core-pgclient
RUN gradle --quiet --no-daemon apache:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "apache/build/libs/http4k-benchmark.jar"]
