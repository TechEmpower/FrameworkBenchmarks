FROM alpine:edge

RUN apk --update --no-cache add gcc git ca-certificates libc-dev openssl1.0-dev h2o-dev wslay-dev crystal shards

WORKDIR /crystal

ENV GC_MARKERS 1

COPY ./ ./

RUN shards install
RUN gcc -shared -O3 lib/h2o/src/ext/h2o.c -I/usr/include -fPIC -o h2o.o
ENV CRYSTAL_PATH=lib:/usr/lib/crystal/core
RUN crystal build --prelude=empty --no-debug --release -Dgc_none -Dfiber_none -Dexcept_none -Dhash_none -Dtime_none -Dregex_none -Dextreme h2o_evloop_hello.cr --link-flags="-Wl,-s $PWD/h2o.o -DH2O_USE_LIBUV=0" -o server.out

CMD sh run.sh
