FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.12.20
WORKDIR /finatra
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt clean assembly -batch

EXPOSE 8888

CMD ["java", "-Dio.netty.recycler.maxCapacityPerThread=0", "-Dio.netty.leakDetection.level=disabled", "-Dcom.twitter.finagle.tracing.enabled=false", "--add-opens=java.base/java.lang=ALL-UNNAMED", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "target/scala-2.12/finatra-benchmark.jar", "-http.response.charset.enabled=false"]
