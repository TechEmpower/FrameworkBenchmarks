FROM gradle:7.4.2-jdk11
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY core core
RUN gradle --quiet apache:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "apache/build/libs/http4k-apache-benchmark.jar"]
