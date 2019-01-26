FROM hseeberger/scala-sbt:11.0.1_2.12.8_1.2.7

WORKDIR /akka-http-slick-postgres

COPY project project
COPY src src
COPY build.sbt .sbtopts .scalafmt.conf ./

RUN \
  hostname && \
  pwd && \
  ls -la && \
  sbt sbtVersion && \
  sbt -batch clean compile stage

CMD ["target/universal/stage/bin/akka-slick-benchmark", "-Dakka.http.benchmark.postgres.dbhost=tfb-database", "-J-d64", "-J-server", "-J-Xms1g", "-J-Xmx1g", "-J-XX:UseG1GC", "-J-XX:ParallelGCThreads=3", "-J-XX:MetaspaceSize=192M", "-J-XX:MaxMetaspaceSize=192M", "-J-XX:+UseStringDeduplication"]
