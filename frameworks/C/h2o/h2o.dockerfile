ARG UBUNTU_VERSION=22.04

ARG H2O_APP_PREFIX=/opt/h2o_app

FROM "ubuntu:${UBUNTU_VERSION}" AS compile

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && \
    apt-get -yqq install \
      autoconf \
      bison \
      cmake \
      curl \
      flex \
      g++ \
      libbrotli-dev \
      libcap-dev \
      libicu-dev \
      libnuma-dev \
      libreadline-dev \
      libssl-dev \
      libtool \
      libuv1-dev \
      libwslay-dev \
      libyajl-dev \
      libz-dev \
      make \
      ninja-build \
      patch \
      pkg-config \
      systemtap-sdt-dev

ARG H2O_VERSION=13ba727ad12dfb2338165d2bcfb2136457e33c8a

WORKDIR /tmp/h2o-build
RUN curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    cmake \
      -B build \
      -DCMAKE_AR=/usr/bin/gcc-ar \
      -DCMAKE_C_FLAGS="-flto -march=native -mtune=native" \
      -DCMAKE_RANLIB=/usr/bin/gcc-ranlib \
      -G Ninja \
      -S . && \
    cmake --build build -j && \
    cmake --install build && \
    cp -a deps/picotls/include/picotls* deps/quicly/include/quicly* /usr/local/include

ARG MUSTACHE_C_REVISION=c1948c599edfe48c6099ed70ab1d5911d8c3ddc8

WORKDIR /tmp/mustache-c-build
RUN curl -LSs "https://github.com/x86-64/mustache-c/archive/${MUSTACHE_C_REVISION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    CFLAGS="-flto -march=native -mtune=native -O3" ./autogen.sh && \
    make -j "$(nproc)" install

ARG POSTGRESQL_VERSION=7b7fa85130330128b404eddebd4f33c6739454b0

WORKDIR /tmp/postgresql-build
RUN curl -LSs "https://github.com/postgres/postgres/archive/${POSTGRESQL_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    curl -LSs "https://www.postgresql.org/message-id/attachment/146614/v2-0001-Add-PQsendSyncMessage-to-libpq.patch" | \
      patch -Np1 && \
    CFLAGS="-flto -march=native -mtune=native -O3" ./configure \
      --includedir=/usr/local/include/postgresql \
      --prefix=/usr/local \
      --with-ssl=openssl && \
    make -j "$(nproc)" -C src/include install && \
    make -j "$(nproc)" -C src/interfaces/libpq install

ARG H2O_APP_PREFIX
WORKDIR /tmp/build
COPY CMakeLists.txt ../
COPY src ../src/
COPY template ../template/
RUN cmake \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_C_FLAGS="-march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_APP_PREFIX}" \
      -G Ninja \
      -S .. && \
    cmake --build . -j && \
    cmake --install .

FROM "ubuntu:${UBUNTU_VERSION}"

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && \
    apt-get -yqq install \
      libnuma1 \
      libyajl2
ARG H2O_APP_PREFIX
COPY --from=compile "${H2O_APP_PREFIX}" "${H2O_APP_PREFIX}/"
COPY --from=compile /usr/local/lib/libmustache_c.so "${H2O_APP_PREFIX}/lib/"
COPY --from=compile /usr/local/lib/libpq.so.5.16 "${H2O_APP_PREFIX}/lib/libpq.so.5"
ENV LD_LIBRARY_PATH="${H2O_APP_PREFIX}/lib"
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD ["taskset", \
     "-c", \
     "0", \
     "/opt/h2o_app/bin/h2o_app", \
     "-a20", \
     "-d", \
     "dbname=hello_world host=tfb-database password=benchmarkdbpass sslmode=disable user=benchmarkdbuser", \
     "-e11000", \
     "-f", \
     "/opt/h2o_app/share/h2o_app/template", \
     "-m1"]
