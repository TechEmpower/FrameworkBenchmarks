FROM hseeberger/scala-sbt:8u265_1.3.13_2.13.3

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

EXPOSE 9000

CMD ["target/universal/stage/bin/akka-http-slick-postgres", "-Dakka.http.benchmark.postgres.dbhost=tfb-database", "-J-d64", "-J-server", "-J-Xms2g", "-J-Xmx2g", "-J-XX:UseG1GC", "-J-XX:ParallelGCThreads=4", "-J-XX:MetaspaceSize=192M", "-J-XX:MaxMetaspaceSize=192M", "-J-XX:+UseStringDeduplication", "-J-Dcom.sun.management.jmxremote=false", "-J-Dcom.sun.management.jmxremote.local.only=false", "-J-Dcom.sun.management.jmxremote.authenticate=false", "-J-Dcom.sun.management.jmxremote.ssl=false", "-J-Djava.awt.headless=true", "-J-Djava.net.preferIPv4Stack=true"]
