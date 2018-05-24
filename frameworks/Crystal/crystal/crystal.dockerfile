FROM crystallang/crystal:0.24.1

WORKDIR /crystal
COPY views views
COPY run.sh run.sh
COPY server.cr server.cr
COPY shard.lock shard.lock
COPY shard.yml shard.yml

ENV GC_MARKERS 1

RUN shards install
RUN crystal build --release --no-debug server.cr -o server.out

CMD bash run.sh
