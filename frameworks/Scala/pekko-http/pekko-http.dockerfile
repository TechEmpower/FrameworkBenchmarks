FROM sbtscala/scala-sbt:eclipse-temurin-focal-17.0.5_8_1.9.3_2.13.11

WORKDIR /pekko-http

RUN mkdir project
COPY pekko-http/project/build.properties project/
COPY pekko-http/project/plugins.sbt project/

RUN \
  which sbt && \
  pwd && \
  ls -la . && \
  ls -la project && \
  sbt sbtVersion

COPY pekko-http/build.sbt pekko-http/.sbtopts ./

RUN sbt update

COPY pekko-http/src src

RUN sbt clean compile stage

EXPOSE 9000

CMD ["target/universal/stage/bin/pekko-http-benchmark", "-Dpekko.http.benchmark.mysql.dbhost=tfb-database", "-J-server", "-J-Xms2g", "-J-Xmx2g", "-J-XX:NewSize=1g", "-J-XX:MaxNewSize=1g", "-J-XX:InitialCodeCacheSize=256m", "-J-XX:ReservedCodeCacheSize=256m", "-J-XX:+UseParallelGC", "-J-XX:+AlwaysPreTouch", "-J-XX:+UseNUMA"]
