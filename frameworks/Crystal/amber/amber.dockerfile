FROM tfb/crystal-lang:latest

ADD ./ /amber
WORKDIR /amber

ENV GC_MARKERS 1
ENV AMBER_ENV production
ENV DATABASE_URL postgres://benchmarkdbuser:benchmarkdbpass@TFB-database:5432/hello_world

RUN apt-get install -y libyaml-dev

RUN shards build amber --release --no-debug

CMD bash run.sh
