FROM gradle:4.6.0-jdk10
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY core core
COPY jetty jetty
COPY netty netty
COPY sunhttp sunhttp
COPY undertow undertow
RUN gradle --quiet build jetty:uber
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "jetty/build/libs/http4k-jetty-benchmark.jar"]
