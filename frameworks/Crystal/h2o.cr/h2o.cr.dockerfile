FROM debian:sid

RUN apt-get update \
  && apt-get install -yqq libh2o-evloop-dev libwslay-dev libyaml-0-2 libevent-dev libpcre3-dev \
    gcc wget git libssl-dev libuv1-dev ca-certificates --no-install-recommends

RUN wget -q https://github.com/crystal-lang/crystal/releases/download/0.26.1/crystal-0.26.1-1-linux-x86_64.tar.gz \
  && tar --strip-components=1 -xzf crystal-0.26.1-1-linux-x86_64.tar.gz -C /usr/ \
  && rm -f *.tar.gz

WORKDIR /crystal

ENV GC_MARKERS 1

COPY ./ ./

RUN shards install
RUN gcc -shared -O3 lib/h2o/src/ext/h2o.c -I/usr/include -fPIC -o h2o.o
ENV CRYSTAL_PATH=lib:/usr/share/crystal/src
RUN crystal build --prelude=empty --no-debug --release -Dgc_none -Dfiber_none -Dexcept_none -Dhash_none -Dtime_none -Dregex_none -Dextreme h2o_evloop_hello.cr --link-flags="-Wl,-s $PWD/h2o.o -DH2O_USE_LIBUV=0" -o server.out

EXPOSE 7890

CMD sh run.sh
