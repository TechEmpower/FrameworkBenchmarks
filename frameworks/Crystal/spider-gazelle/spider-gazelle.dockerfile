FROM crystallang/crystal:0.27.0
WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src

# Build App
RUN shards build --release --no-debug

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=56&max_pool_size=56&max_idle_pool_size=56
ENV SG_ENV production

# Run the app binding on port 8080
EXPOSE 8080
CMD bin/app -w $(nproc) -b 0.0.0.0 -p 8080
