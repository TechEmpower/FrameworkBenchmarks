FROM crystallang/crystal:0.26.1

WORKDIR /raze
COPY views views
COPY run.sh run.sh
COPY raze.cr raze.cr
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_pool_size=56&max_idle_pool_size=56

RUN shards install
RUN crystal build --release --no-debug raze.cr

EXPOSE 8080

CMD bash run.sh
