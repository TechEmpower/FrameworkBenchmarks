FROM crystallang/crystal:1.0.0

WORKDIR /grip
COPY views views
COPY run.sh run.sh
COPY grip.cr grip.cr
COPY shard.yml shard.yml

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_idle_pool_size=56
ENV GRIP_ENV production

RUN shards install
RUN crystal build  --release --no-debug grip.cr

EXPOSE 8080

CMD bash run.sh
