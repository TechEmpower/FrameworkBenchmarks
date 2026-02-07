FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.13.16 as build

WORKDIR /pekko-http
COPY pekko-http ./
RUN \
  which sbt && \
  pwd && \
  ls -la . && \
  ls -la project && \
  sbt sbtVersion
RUN sbt clean compile stage

FROM eclipse-temurin:21-jre-noble
COPY --from=build /pekko-http/target/universal/stage /pekko-http

EXPOSE 9000

CMD ["/pekko-http/bin/pekko-http-benchmark", "-Dpekko.http.benchmark.mysql.dbhost=tfb-database", "-J-server", "-J-Xms2g", "-J-Xmx2g", "-J-XX:NewSize=1g", "-J-XX:MaxNewSize=1g", "-J-XX:InitialCodeCacheSize=256m", "-J-XX:ReservedCodeCacheSize=256m", "-J-XX:+UseParallelGC", "-J-XX:+AlwaysPreTouch", "-J-XX:+UseNUMA"]
