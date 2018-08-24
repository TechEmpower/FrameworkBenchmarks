FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /blaze
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch
CMD ["java", "-server", "-Xms2g", "-Xmx2g", "-XX:NewSize=1g", "-XX:MaxNewSize=1g", "-XX:InitialCodeCacheSize=256m", "-XX:ReservedCodeCacheSize=256m", "-XX:+UseParallelGC", "-XX:+UseNUMA", "-XX:+AggressiveOpts", "-XX:-UseBiasedLocking", "-XX:+AlwaysPreTouch", "-jar", "target/scala-2.12/blaze-assembly-1.0.jar"]
