FROM crystallang/crystal:0.27.2

WORKDIR /onyx
COPY run.sh run.sh
COPY src src
COPY shard.yml shard.yml

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_pool_size=56&max_idle_pool_size=56
ENV TEST_HOST tfb-server
ENV CRYSTAL_ENV benchmarking

RUN shards install
RUN crystal build --release --no-debug src/server_postgres.cr

EXPOSE 8080
CMD bash run.sh
