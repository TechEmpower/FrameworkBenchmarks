FROM crystallang/crystal:0.27.1

WORKDIR /onyx
COPY run.sh run.sh
COPY src src
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_pool_size=56&max_idle_pool_size=56
ENV TEST_HOST tfb-server

RUN shards install
RUN crystal build --no-debug src/server_postgres.cr

EXPOSE 8080
CMD bash run.sh
