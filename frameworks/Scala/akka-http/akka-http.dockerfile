FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /akka-http
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt -batch clean compile stage
CMD ["target/universal/stage/bin/akka-http-benchmark", "-Dakka.http.benchmark.mysql.dbhost=tfb-database", "-J-server", "-J-Xms2g", "-J-Xmx2g", "-J-XX:NewSize=1g", "-J-XX:MaxNewSize=1g", "-J-XX:InitialCodeCacheSize=256m", "-J-XX:ReservedCodeCacheSize=256m", "-J-XX:+UseParallelGC", "-J-XX:-UseBiasedLocking", "-J-XX:+AlwaysPreTouch"]
