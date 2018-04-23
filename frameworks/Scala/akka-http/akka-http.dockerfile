FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /akka-http
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt -batch 'universal:stage'
CMD ["target/universal/stage/bin/akka-http-benchmark", "-Dakka.http.benchmark.mysql.dbhost=tfb-database"]
