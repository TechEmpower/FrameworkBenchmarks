FROM hseeberger/scala-sbt:8u181_2.12.7_1.2.6
WORKDIR /http4s
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:NewSize=1g", "-XX:MaxNewSize=1g", "-XX:InitialCodeCacheSize=256m", "-XX:ReservedCodeCacheSize=256m", "-XX:+UseParallelGC", "-XX:+UseNUMA", "-XX:+AggressiveOpts", "-XX:-UseBiasedLocking", "-XX:+AlwaysPreTouch", "-jar", "target/scala-2.12/http4s-assembly-1.0.jar", "tfb-database"]
