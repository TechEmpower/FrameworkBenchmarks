FROM crystallang/crystal:0.34.0

WORKDIR /orion
COPY views views
COPY run.sh run.sh
COPY orion.cr orion.cr
COPY shard.yml shard.yml

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_idle_pool_size=56

RUN shards install
RUN crystal build  --release --no-debug orion.cr

EXPOSE 8080

CMD bash run.sh
