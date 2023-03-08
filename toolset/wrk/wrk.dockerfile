ARG UBUNTU_VERSION=jammy

FROM "buildpack-deps:${UBUNTU_VERSION}" AS compile

ARG WRK_VERSION=4.2.0

ARG LDFLAGS="-flto -march=native -mtune=native -O3"
ARG CFLAGS="-I /usr/include/luajit-2.1 ${LDFLAGS}"
ARG DEBIAN_FRONTEND=noninteractive
WORKDIR /tmp
RUN apt-get -yqq update && \
    apt-get -yqq install \
      libluajit-5.1-dev \
      libssl-dev \
      luajit && \
    curl -LSs "https://github.com/wg/wrk/archive/${WRK_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    make WITH_LUAJIT=/usr WITH_OPENSSL=/usr -j "$(nproc)"

FROM "ubuntu:${UBUNTU_VERSION}"

# Required scripts for benchmarking
COPY concurrency.sh pipeline.lua pipeline.sh query.sh ./

ARG DEBIAN_FRONTEND=noninteractive
COPY --from=compile /tmp/wrk /usr/local/bin/
RUN apt-get -yqq update && \
    apt-get -yqq install \
      curl \
      libluajit-5.1-2 && \
    chmod 777 concurrency.sh pipeline.sh query.sh

# Environment vars required by the wrk scripts with nonsense defaults
ENV accept=accept \
    duration=duration \
    levels=levels \
    max_concurrency=max_concurrency \
    max_threads=max_threads \
    name=name \
    pipeline=pipeline \
    server_host=server_host
