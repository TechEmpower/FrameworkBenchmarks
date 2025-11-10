FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.12.20
WORKDIR /finch
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 9000

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dio.netty.recycler.maxCapacityPerThread=0", "-Dcom.twitter.finagle.tracing.enabled=false", "-Dio.netty.leakDetection.level=disabled", "-jar", "target/scala-2.12/finch-benchmark.jar"]
