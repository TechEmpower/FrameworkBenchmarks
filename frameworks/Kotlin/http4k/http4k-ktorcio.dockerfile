FROM gradle:4.7.0-jdk10
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY core core
COPY jetty jetty
COPY ktorcio ktorcio
COPY netty netty
COPY sunhttp sunhttp
COPY undertow undertow
RUN gradle --quiet build ktorcio:shadowJar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "ktorcio/build/libs/http4k-ktorcio-benchmark.jar"]
