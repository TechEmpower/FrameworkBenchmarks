ARG UBUNTU_VERSION=25.10

ARG H2O_PREFIX=/opt/h2o

FROM "ubuntu:${UBUNTU_VERSION}" AS compile

ARG H2O_VERSION=3b9b6a53cac8bcc6a25fb28df81ad295fc5f9402

ARG DEBIAN_FRONTEND=noninteractive
ARG H2O_PREFIX
WORKDIR /tmp/h2o-build
RUN apt-get -yqq update && \
    apt-get -yqq install \
      cmake \
      curl \
      g++ \
      libbrotli-dev \
      libcap-dev \
      libssl-dev \
      liburing-dev \
      libuv1-dev \
      libwslay-dev \
      libz-dev \
      ninja-build \
      pkg-config \
      ruby \
      systemtap-sdt-dev && \
    curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    cmake \
      -B build \
      -DCMAKE_AR=/usr/bin/gcc-ar \
      -DCMAKE_C_FLAGS="-flto=auto -march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_PREFIX}" \
      -DCMAKE_RANLIB=/usr/bin/gcc-ranlib \
      -DWITH_MRUBY=on \
      -G Ninja \
      -S . && \
    cmake --build build -j && \
    cmake --install build

FROM "ubuntu:${UBUNTU_VERSION}"

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && apt-get -yqq install liburing2
ARG H2O_PREFIX
COPY --from=compile "${H2O_PREFIX}/bin/h2o" "${H2O_PREFIX}/bin/"
COPY --from=compile "${H2O_PREFIX}/share" "${H2O_PREFIX}/share/"
COPY h2o.conf "${H2O_PREFIX}/etc/"
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD ["/opt/h2o/bin/h2o", "-c", "/opt/h2o/etc/h2o.conf"]
