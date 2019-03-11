FROM hseeberger/scala-sbt:8u181_2.12.6_1.2.3

WORKDIR /akka-http-slick-postgres

RUN mkdir project
COPY akka-http-slick-postgres/project/build.properties project/
COPY akka-http-slick-postgres/project/plugins.sbt project/

RUN \
  which sbt && \
  pwd && \
  ls -la . && \
  ls -la project && \
  sbt sbtVersion

COPY akka-http-slick-postgres/build.sbt akka-http-slick-postgres/.sbtopts akka-http-slick-postgres/.scalafmt.conf ./

RUN sbt update

COPY akka-http-slick-postgres/src src

RUN sbt clean compile stage

CMD ["target/universal/stage/bin/akka-http-slick-postgres", "-Dakka.http.benchmark.postgres.dbhost=tfb-database", "-J-d64", "-J-server", "-J-Xms1g", "-J-Xmx1g", "-J-XX:UseG1GC", "-J-XX:ParallelGCThreads=3", "-J-XX:MetaspaceSize=192M", "-J-XX:MaxMetaspaceSize=192M", "-J-XX:+UseStringDeduplication"]
