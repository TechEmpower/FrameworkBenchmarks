FROM gradle:7.6-jdk19
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY core-pgclient core-pgclient
COPY jettyloom-pgclient jettyloom-pgclient
RUN gradle --quiet --no-daemon jettyloom-pgclient:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "--enable-preview", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "jettyloom-pgclient/build/libs/http4k-benchmark.jar"]
