FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /finatra
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt clean assembly -batch

EXPOSE 8888

CMD ["java", "-Dio.netty.recycler.maxCapacityPerThread=0", "-Dio.netty.leakDetection.level=disabled", "-Dcom.twitter.finagle.tracing.enabled=false", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "target/scala-2.12/finatra-benchmark.jar", "-http.response.charset.enabled=false"]
