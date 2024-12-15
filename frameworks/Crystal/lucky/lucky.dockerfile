FROM crystallang/crystal:1.14.0

WORKDIR /lucky
COPY shard.lock shard.lock
COPY shard.yml shard.yml
RUN shards install

COPY config config
COPY src src
COPY run.sh run.sh

ENV GC_MARKERS 1
ENV LUCKY_ENV production

RUN shards build bench --release --no-debug

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=10&max_idle_pool_size=10

EXPOSE 8080

CMD bash run.sh
