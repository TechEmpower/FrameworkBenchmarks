ARG RUBY_IMAGE_VERSION=3.2

ARG H2O_PREFIX=/opt/h2o

FROM "ruby:${RUBY_IMAGE_VERSION}" AS compile

ARG H2O_VERSION=9ab3feb4d7429ddda52a3cf84bd6da0e890bd52a

ARG DEBIAN_FRONTEND=noninteractive
ARG H2O_PREFIX
WORKDIR /tmp/h2o-build
RUN apt-get -yqq update && \
    apt-get -yqq install \
      cmake \
      libcap-dev \
      libuv1-dev \
      libwslay-dev \
      ninja-build \
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

FROM ruby:${RUBY_IMAGE_VERSION}-slim

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && apt-get -yqq install libcap2
ARG H2O_PREFIX
COPY --from=compile "${H2O_PREFIX}" "${H2O_PREFIX}/"
COPY h2o.conf "${H2O_PREFIX}/etc/"
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD ["/opt/h2o/bin/h2o", "-c", "/opt/h2o/etc/h2o.conf"]
