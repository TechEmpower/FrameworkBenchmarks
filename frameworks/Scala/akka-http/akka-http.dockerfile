FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.13.16

WORKDIR /akka-http

RUN mkdir project
COPY akka-http/project/build.properties project/
COPY akka-http/project/plugins.sbt project/

RUN \
  which sbt && \
  pwd && \
  ls -la . && \
  ls -la project && \
  sbt sbtVersion

COPY akka-http/build.sbt akka-http/.sbtopts ./

RUN sbt update

COPY akka-http/src src

RUN sbt clean compile stage

EXPOSE 9000

CMD ["target/universal/stage/bin/akka-http-benchmark", "-Dakka.http.benchmark.mysql.dbhost=tfb-database", "-J-server", "-J-Xms2g", "-J-Xmx2g", "-J-XX:NewSize=1g", "-J-XX:MaxNewSize=1g", "-J-XX:InitialCodeCacheSize=256m", "-J-XX:ReservedCodeCacheSize=256m", "-J-XX:+UseParallelGC", "-J-XX:+AlwaysPreTouch", "-J-XX:+UseNUMA"]
