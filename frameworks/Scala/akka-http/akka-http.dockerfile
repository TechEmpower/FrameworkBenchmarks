FROM techempower/sbt:0.1

COPY ./ ./

RUN sbt -batch 'universal:stage'

CMD ./target/universal/stage/bin/akka-http-benchmark \
    -Dakka.http.benchmark.mysql.dbhost=TFB-database
