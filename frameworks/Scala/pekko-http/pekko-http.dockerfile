FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.5_8_1.9.3_2.13.11 as build

WORKDIR /pekko-http
COPY pekko-http ./
RUN \
  which sbt && \
  pwd && \
  ls -la . && \
  ls -la project && \
  sbt sbtVersion
RUN sbt clean compile stage

FROM eclipse-temurin:17-jre-jammy
COPY --from=build /pekko-http/target/universal/stage /pekko-http

EXPOSE 9000

CMD ["/pekko-http/bin/pekko-http-benchmark", "-Dpekko.http.benchmark.mysql.dbhost=tfb-database", "-J-server", "-J-Xms2g", "-J-Xmx2g", "-J-XX:NewSize=1g", "-J-XX:MaxNewSize=1g", "-J-XX:InitialCodeCacheSize=256m", "-J-XX:ReservedCodeCacheSize=256m", "-J-XX:+UseParallelGC", "-J-XX:+AlwaysPreTouch", "-J-XX:+UseNUMA"]
