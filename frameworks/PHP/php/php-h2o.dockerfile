ARG UBUNTU_VERSION=22.04

ARG H2O_PREFIX=/opt/h2o

FROM "ubuntu:${UBUNTU_VERSION}" AS compile

ARG H2O_VERSION=9ab3feb4d7429ddda52a3cf84bd6da0e890bd52a

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
      libtool \
      libuv1-dev \
      libwslay-dev \
      libz-dev \
      ninja-build \
      pkg-config \
      systemtap-sdt-dev && \
    curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    cmake \
      -B build \
      -DCMAKE_AR=/usr/bin/gcc-ar \
      -DCMAKE_C_FLAGS="-flto -march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_PREFIX}" \
      -DCMAKE_RANLIB=/usr/bin/gcc-ranlib \
      -G Ninja \
      -S . && \
    cmake --build build -j && \
    cmake --install build

FROM "ubuntu:${UBUNTU_VERSION}"

ARG PHP_VERSION=8.2

ENV TZ=America/Los_Angeles

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && \
    apt-get -yqq install \
      apt-utils \
      software-properties-common && \
    LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php && \
    apt-get -yqq update && \
    apt-get -yqq install \
      "php${PHP_VERSION}" \
      "php${PHP_VERSION}-cli" \
      "php${PHP_VERSION}-common" \
      "php${PHP_VERSION}-fpm" \
      "php${PHP_VERSION}-mysql"
ARG H2O_PREFIX
COPY --from=compile "${H2O_PREFIX}" "${H2O_PREFIX}/"
COPY deploy/conf "/etc/php/${PHP_VERSION}/fpm/"
RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" \
      "/etc/php/${PHP_VERSION}/fpm/php-fpm.conf"; fi;
COPY deploy/h2o.conf "${H2O_PREFIX}/etc/"
COPY *.php /var/www/
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD sed -i "s/num-threads: x/num-threads: $((2 * $(nproc)))/g" /opt/h2o/etc/h2o.conf && \
    service php8.2-fpm start && \
    /opt/h2o/bin/h2o -c /opt/h2o/etc/h2o.conf
