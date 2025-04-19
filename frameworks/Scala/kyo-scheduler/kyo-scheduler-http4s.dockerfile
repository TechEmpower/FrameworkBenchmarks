FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.13.16

WORKDIR /kyo-scheduler-benchmark
COPY http4s http4s
COPY project project
COPY build.sbt build.sbt
RUN sbt http4s/assembly

EXPOSE 8080

CMD java \
      -server \
      -Xms2g \
      -Xmx2g \
      -XX:NewSize=1g \
      -XX:MaxNewSize=1g \
      -XX:InitialCodeCacheSize=256m \
      -XX:ReservedCodeCacheSize=256m \
      -XX:+UseParallelGC \
      -XX:+AlwaysPreTouch \
      -Dcats.effect.stackTracingMode=disabled \
      -jar \
      /kyo-scheduler-benchmark/http4s/target/scala-2.13/http4s-kyo-scheduler-benchmark-assembly-1.0.0.jar \
      tfb-database
