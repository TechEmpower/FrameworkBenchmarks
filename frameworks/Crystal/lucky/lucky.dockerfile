FROM crystallang/crystal:0.35.1

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

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_idle_pool_size=56

EXPOSE 8080

CMD bash run.sh
