FROM placeos/crystal
WORKDIR /usr/src/app

COPY shard.yml ./
COPY src src

# Build App
RUN shards build --release --no-debug

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world

ENV SG_ENV production

# Run the app binding on port 8080
EXPOSE 8080
ENTRYPOINT []

CMD bin/app -w $(nproc) -b 0.0.0.0 -p 8080
