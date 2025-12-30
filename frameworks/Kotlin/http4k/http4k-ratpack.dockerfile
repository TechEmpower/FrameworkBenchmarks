FROM gradle:8.7.0-jdk21
USER root
WORKDIR /http4k
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY core core
COPY core-jdbc core-jdbc
COPY core-pgclient core-pgclient
COPY ratpack ratpack
RUN gradle --quiet --no-daemon ratpack:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "ratpack/build/libs/http4k-benchmark.jar"]
