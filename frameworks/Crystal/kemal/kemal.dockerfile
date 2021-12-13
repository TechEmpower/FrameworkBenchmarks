FROM crystallang/crystal:0.32.1

WORKDIR /kemal
COPY views views
COPY run.sh run.sh
COPY server-postgres.cr server-postgres.cr
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV GC_MARKERS 1
ENV KEMAL_ENV production
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_idle_pool_size=56

RUN shards install
RUN crystal build --release --no-debug server-postgres.cr

EXPOSE 8080

CMD bash run.sh
