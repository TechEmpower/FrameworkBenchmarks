FROM crystallang/crystal:1.14.0

WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src
COPY run.sh run.sh

# Build App
RUN shards build --release --no-debug

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=5&max_idle_pool_size=5

ENV SG_ENV production

# Run the app binding on port 8080
EXPOSE 8080
ENTRYPOINT []

CMD bash run.sh
