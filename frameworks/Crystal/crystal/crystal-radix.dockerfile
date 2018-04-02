FROM techempower/crystal-lang:0.1

ADD ./ /crystal
WORKDIR /crystal

ENV GC_MARKERS 1

RUN shards install
RUN crystal build --release --no-debug server_radix.cr -o server_radix.out

CMD bash run-radix.sh
