FROM nimlang/nim:1.0.4

ADD ./ /jester
WORKDIR /jester
RUN nimble c -d:release --threads:on -y techempower.nim

ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world?initial_pool_size=128&max_idle_pool_size=128

CMD ./techempower
