FROM gradle:7.5.1-jdk17
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY undertow undertow
RUN gradle --quiet undertow:shadowJar

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "undertow/build/libs/http4k-undertow-benchmark.jar"]
