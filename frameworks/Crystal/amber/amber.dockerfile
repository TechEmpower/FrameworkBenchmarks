FROM crystallang/crystal:0.26.1

WORKDIR /amber
COPY config config
COPY src src
COPY run.sh run.sh
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV GC_MARKERS 1
ENV AMBER_ENV production
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_pool_size=56&max_idle_pool_size=56

RUN apt-get install -yqq libyaml-dev
RUN shards build amber --release --no-debug

EXPOSE 8080

CMD bash run.sh
