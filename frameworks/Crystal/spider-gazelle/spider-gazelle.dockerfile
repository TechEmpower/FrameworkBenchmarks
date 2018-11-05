FROM crystallang/crystal:0.26.1
ADD . /src
WORKDIR /src

# Build App
RUN shards build --production

# Extract dependencies
RUN ldd bin/app | tr -s '[:blank:]' '\n' | grep '^/' | \
    xargs -I % sh -c 'mkdir -p $(dirname deps%); cp % deps%;'

# Build a minimal docker image
FROM scratch
COPY --from=0 /src/deps /
COPY --from=0 /src/bin/app /app

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world
ENV SG_ENV production

# Run the app binding on port 8080
EXPOSE 8080
ENTRYPOINT ["/app"]
CMD ["/app", "-b", "0.0.0.0", "-p", "8080", "-w", "0"]
