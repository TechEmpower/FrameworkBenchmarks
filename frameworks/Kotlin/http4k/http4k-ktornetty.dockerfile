FROM gradle:5.4.1-jdk11
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY core core
COPY jetty jetty
COPY ktorcio ktorcio
COPY ktornetty ktornetty
COPY netty netty
COPY undertow undertow
RUN gradle --quiet build ktornetty:shadowJar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "ktornetty/build/libs/http4k-ktornetty-benchmark.jar"]
