FROM gradle:6.6.0-jdk11
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY core core
COPY netty netty
RUN gradle --quiet netty:shadowJar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "netty/build/libs/http4k-netty-benchmark.jar"]
