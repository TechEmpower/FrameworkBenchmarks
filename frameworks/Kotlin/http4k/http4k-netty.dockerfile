FROM gradle:7.6-jdk19
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY core-jdbc core-jdbc
COPY core-pgclient core-pgclient
COPY netty netty
RUN gradle --quiet --no-daemon netty:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AlwaysPreTouch", "-jar", "netty/build/libs/http4k-benchmark.jar"]
