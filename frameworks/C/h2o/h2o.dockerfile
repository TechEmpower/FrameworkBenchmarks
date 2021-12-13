FROM ubuntu:20.04

WORKDIR /h2o_app_src
COPY ./ ./

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get install -yqq autoconf bison cmake curl file flex g++ git libnuma-dev libpq-dev \
                         libssl-dev libtool libyajl-dev libz-dev make ninja-build wget

### Install mustache-c

ARG MUSTACHE_C_REVISION=c1948c599edfe48c6099ed70ab1d5911d8c3ddc8

ARG MUSTACHE_C_BUILD_DIR=mustache-c-build
ENV MUSTACHE_C_PREFIX /opt/mustache-c

RUN mkdir -p "$MUSTACHE_C_BUILD_DIR" && \
    cd "$MUSTACHE_C_BUILD_DIR" && \
    wget -qO - "https://github.com/x86-64/mustache-c/archive/${MUSTACHE_C_REVISION}.tar.gz" | \
    tar xz --strip-components=1 && \
    CFLAGS="-O3 -flto -march=native" ./autogen.sh --prefix="$MUSTACHE_C_PREFIX" && \
    make -j "$(nproc)" install && \
    cd .. && \
    rm -rf "$MUSTACHE_C_BUILD_DIR"

### Install h2o

ARG H2O_VERSION=v2.2.6

ARG H2O_BUILD_DIR=h2o-build
ENV H2O_PREFIX /opt/h2o

RUN mkdir -p "${H2O_BUILD_DIR}/build" && \
    cd "$H2O_BUILD_DIR" && \
    wget -qO - "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
    tar xz --strip-components=1 && \
    cd build && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_PREFIX" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -G Ninja .. && \
    cmake --build . -j && \
    cmake --install . && \
    cd ../.. && \
    rm -rf "$H2O_BUILD_DIR"

EXPOSE 8080

CMD ["./h2o.sh"]
