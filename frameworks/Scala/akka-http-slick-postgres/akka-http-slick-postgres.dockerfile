FROM hseeberger/scala-sbt:11.0.1_2.12.8_1.2.7

ENV SCALA_VERSION 2.12.8
ENV SBT_VERSION 1.2.7

WORKDIR /akka-http-slick-postgres

RUN mkdir project
COPY project/build.properties project/
COPY project/plugins.sbt project/

RUN \
  which sbt && \
  pwd && \
  ls -la . && \
  ls -la project && \
  sbt sbtVersion

COPY build.sbt .sbtopts .scalafmt.conf ./

RUN sbt update

COPY src src

RUN sbt clean compile stage

CMD ["target/universal/stage/bin/akka-slick-benchmark", "-Dakka.http.benchmark.postgres.dbhost=tfb-database", "-J-d64", "-J-server", "-J-Xms1g", "-J-Xmx1g", "-J-XX:UseG1GC", "-J-XX:ParallelGCThreads=3", "-J-XX:MetaspaceSize=192M", "-J-XX:MaxMetaspaceSize=192M", "-J-XX:+UseStringDeduplication"]
