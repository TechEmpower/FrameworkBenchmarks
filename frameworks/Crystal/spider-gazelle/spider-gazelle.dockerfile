FROM crystallang/crystal:0.27.0
ADD . /src
WORKDIR /src

# Build App
RUN shards build --production

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world
ENV SG_ENV production

# Run the app binding on port 8080
EXPOSE 8080
ENTRYPOINT ["/src/bin/app"]
CMD ["/src/bin/app", "-b", "0.0.0.0", "-p", "8080", "-w", "0"]
