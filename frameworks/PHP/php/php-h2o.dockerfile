ARG UBUNTU_VERSION=24.04

ARG H2O_PREFIX=/opt/h2o

FROM "buildpack-deps:${UBUNTU_VERSION}" AS compile

ARG H2O_VERSION=3b9b6a53cac8bcc6a25fb28df81ad295fc5f9402

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
      libwslay-dev \
      libz-dev \
      make \
      ninja-build \
      pkg-config \
      ruby \
      systemtap-sdt-dev > /dev/null && \
    curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz > /dev/null && \
    cmake \
      -B build \
      -DCMAKE_C_FLAGS="-flto=auto -march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_PREFIX}" \
      -DWITH_MRUBY=on \
      -G Ninja \
      -S . > /dev/null && \
    cmake --build build -j > /dev/null && \
    cmake --install build > /dev/null

FROM "ubuntu:${UBUNTU_VERSION}"

ARG PHP_VERSION=8.4

ENV TZ=America/Los_Angeles

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install \
      --no-install-recommends \
      -qqUy \
      apt-utils \
      software-properties-common > /dev/null && \
    LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php && \
    apt-get install \
      --no-install-recommends \
      -qqUy \
      liburing2 \
      "php${PHP_VERSION}" \
      "php${PHP_VERSION}-cli" \
      "php${PHP_VERSION}-common" \
      "php${PHP_VERSION}-fpm" \
      "php${PHP_VERSION}-mysql" > /dev/null
ARG H2O_PREFIX
COPY --from=compile "${H2O_PREFIX}/bin/h2o" "${H2O_PREFIX}/bin/"
COPY --from=compile "${H2O_PREFIX}/share" "${H2O_PREFIX}/share/"
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
    service php8.4-fpm start && \
    /opt/h2o/bin/h2o -c /opt/h2o/etc/h2o.conf
