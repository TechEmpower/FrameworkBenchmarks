FROM gradle:7.6-jdk19
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY core-pgclient core-pgclient
COPY sunhttploom sunhttploom
RUN gradle --quiet --no-daemon sunhttploom:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "--enable-preview", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "sunhttploom/build/libs/http4k-benchmark.jar"]
