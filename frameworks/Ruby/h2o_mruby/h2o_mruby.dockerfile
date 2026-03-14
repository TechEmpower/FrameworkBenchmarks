ARG UBUNTU_VERSION=26.04

ARG H2O_PREFIX=/opt/h2o

FROM "buildpack-deps:${UBUNTU_VERSION}" AS compile

ARG H2O_VERSION=ccea64b17ade832753db933658047ede9f31a380

ARG DEBIAN_FRONTEND=noninteractive
ARG H2O_PREFIX
WORKDIR /tmp/h2o-build
RUN apt-get install \
      --no-install-recommends \
      -qqUy \
      cmake \
      curl \
      g++ \
      libbrotli-dev \
      libcap-dev \
      libssl-dev \
      liburing-dev \
      libuv1-dev \
      libz-dev \
      make \
      pkg-config \
      ruby \
      systemtap-sdt-dev > /dev/null && \
    curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz > /dev/null && \
    cmake \
      -B build \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_C_FLAGS="-flto=auto -march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_PREFIX}" \
      -DWITH_MRUBY=on \
      -S . > /dev/null && \
    cmake --build build -j > /dev/null && \
    cmake --install build > /dev/null

FROM "ubuntu:${UBUNTU_VERSION}"

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install --no-install-recommends -qqUy liburing2
ARG H2O_PREFIX
COPY --from=compile "${H2O_PREFIX}/bin/h2o" "${H2O_PREFIX}/bin/"
COPY --from=compile "${H2O_PREFIX}/share" "${H2O_PREFIX}/share/"
COPY h2o.conf "${H2O_PREFIX}/etc/"
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD ["/opt/h2o/bin/h2o", "-c", "/opt/h2o/etc/h2o.conf"]
