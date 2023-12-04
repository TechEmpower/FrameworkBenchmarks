ARG UBUNTU_VERSION=22.04

ARG H2O_PREFIX=/opt/h2o

FROM "ubuntu:${UBUNTU_VERSION}" AS compile

ARG H2O_VERSION=18b175f71ede08b50d3e5ae8303dacef3ea510fc

ARG DEBIAN_FRONTEND=noninteractive
ARG H2O_PREFIX
WORKDIR /tmp/h2o-build
RUN apt-get -yqq update && \
    apt-get -yqq install \
      cmake \
      curl \
      g++ \
      libbpfcc-dev \
      libbrotli-dev \
      libcap-dev \
      libssl-dev \
      libtool \
      libuv1-dev \
      libwslay-dev \
      libz-dev \
      ninja-build \
      pkg-config \
      rsync \
      ruby \
      systemtap-sdt-dev && \
    curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    cmake \
      -B build \
      -DCMAKE_AR=/usr/bin/gcc-ar \
      -DCMAKE_C_FLAGS="-flto -march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_PREFIX}" \
      -DCMAKE_RANLIB=/usr/bin/gcc-ranlib \
      -DWITH_MRUBY=on \
      -G Ninja \
      -S . && \
    cmake --build build -j && \
    cmake --install build

FROM "ubuntu:${UBUNTU_VERSION}"

ARG H2O_PREFIX
COPY --from=compile "${H2O_PREFIX}/bin/h2o" "${H2O_PREFIX}/bin/"
COPY --from=compile "${H2O_PREFIX}/share" "${H2O_PREFIX}/share/"
COPY h2o.conf "${H2O_PREFIX}/etc/"
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD ["/opt/h2o/bin/h2o", "-c", "/opt/h2o/etc/h2o.conf"]
