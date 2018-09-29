FROM crystallang/crystal:0.26.1

WORKDIR /amber
COPY config config
COPY src src
COPY run.sh run.sh
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV GC_MARKERS 1
ENV AMBER_ENV production
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

RUN apt install -yqq libyaml-dev
RUN shards build amber --release --no-debug

CMD bash run.sh
