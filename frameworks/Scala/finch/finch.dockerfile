FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /finch
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dio.netty.recycler.maxCapacityPerThread=0", "-Dcom.twitter.finagle.tracing.enabled=false", "-Dio.netty.leakDetection.level=disabled", "-jar", "target/scala-2.12/finch-benchmark.jar"]
