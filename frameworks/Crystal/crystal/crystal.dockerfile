FROM crystallang/crystal:1.0.0

WORKDIR /crystal
COPY shard.yml shard.yml
COPY shard.lock shard.lock
RUN shards install

COPY views views
COPY run.sh run.sh
COPY server.cr server.cr

ENV GC_MARKERS 1
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_idle_pool_size=56

RUN crystal build --release --no-debug server.cr -o server.out

EXPOSE 8080

CMD bash run.sh
