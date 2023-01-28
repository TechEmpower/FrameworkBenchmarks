FROM buildpack-deps:jammy

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && apt-get -yqq install libluajit-5.1-dev libssl-dev luajit

WORKDIR /wrk
RUN curl -sL https://github.com/wg/wrk/archive/4.2.0.tar.gz | tar xz --strip-components=1
ARG LDFLAGS="-O3 -march=native -mtune=native -flto"
ARG CFLAGS="-I /usr/include/luajit-2.1 ${LDFLAGS}"
RUN make WITH_LUAJIT=/usr WITH_OPENSSL=/usr -j "$(nproc)"
RUN cp wrk /usr/local/bin

WORKDIR /
# Required scripts for benchmarking
COPY pipeline.lua pipeline.lua
COPY concurrency.sh concurrency.sh
COPY pipeline.sh pipeline.sh
COPY query.sh query.sh

RUN chmod 777 pipeline.lua concurrency.sh pipeline.sh query.sh

# Environment vars required by the wrk scripts with nonsense defaults
ENV name=name
ENV server_host=server_host
ENV levels=levels
ENV duration=duration
ENV max_concurrency=max_concurrency
ENV max_threads=max_threads
ENV pipeline=pipeline
ENV accept=accept
