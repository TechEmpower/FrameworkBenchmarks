FROM crystallang/crystal:1.12.1

WORKDIR /grip
COPY views views
COPY run.sh run.sh
COPY server-postgres.cr server-postgres.cr
COPY shard.yml shard.yml

ENV GC_MARKERS 1
ENV ENVIRONMENT PRODUCTION
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=128&max_idle_pool_size=128

RUN shards install
RUN crystal build --release --no-debug server-postgres.cr

EXPOSE 8080

CMD bash run.sh
