FROM crystallang/crystal:0.26.1

WORKDIR /crystal
COPY views views
COPY run-radix.sh run-radix.sh
COPY server_radix.cr server_radix.cr
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV GC_MARKERS 1
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=8&max_pool_size=8&max_idle_pool_size=8

RUN shards install
RUN crystal build --release --no-debug server_radix.cr -o server_radix.out

CMD bash run-radix.sh
