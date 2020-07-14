FROM gradle:5.4.1-jdk11
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY core core
COPY jetty jetty
COPY ktorcio ktorcio
COPY netty netty
COPY ratpack ratpack
COPY undertow undertow
RUN gradle --quiet build jetty:shadowJar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "jetty/build/libs/http4k-jetty-benchmark.jar"]
