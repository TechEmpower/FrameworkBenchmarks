FROM gradle:7.6-jdk19
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY core-jdbc core-jdbc
COPY core-pgclient core-pgclient
COPY ratpack ratpack
RUN gradle --quiet --no-daemon ratpack:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "ratpack/build/libs/http4k-benchmark.jar"]
