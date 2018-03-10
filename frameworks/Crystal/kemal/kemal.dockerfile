FROM tfb/crystal-lang:latest

ADD ./ /kemal
WORKDIR /kemal

ENV GC_MARKERS 1
ENV KEMAL_ENV production

RUN shards install
RUN crystal build --release --no-debug server-postgres.cr

CMD bash run.sh
