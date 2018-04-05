FROM techempower/sbt:0.1

ADD ./ /akka-http
WORKDIR /akka-http

RUN sbt -batch 'universal:stage'

CMD ./target/universal/stage/bin/akka-http-benchmark \
    -Dakka.http.benchmark.mysql.dbhost=tfb-database
