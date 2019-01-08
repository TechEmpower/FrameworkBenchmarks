FROM gradle:4.7.0-jdk10
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY core core
COPY jetty jetty
COPY ktor-cio ktor-cio
COPY netty netty
COPY sunhttp sunhttp
COPY undertow undertow
RUN gradle --quiet build ktor-cio:shadowJar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "ktor-cio/build/libs/http4k-ktor-cio-benchmark.jar"]
