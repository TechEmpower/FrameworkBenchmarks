FROM crystallang/crystal:0.34.0

WORKDIR /toro
COPY views views
COPY run.sh run.sh
COPY toro.cr toro.cr
COPY shard.yml shard.yml

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

RUN shards install
RUN crystal build  --release --no-debug toro.cr

EXPOSE 8080

CMD bash run.sh
