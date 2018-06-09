FROM crystallang/crystal:0.24.1

WORKDIR /raze
COPY views views
COPY run.sh run.sh
COPY raze.cr raze.cr
COPY shard.lock shard.lock
COPY shard.yml shard.yml

RUN shards install
RUN crystal build --release --no-debug raze.cr

CMD bash run.sh
