FROM ubuntu:16.04

COPY ./ ./

RUN apt-get update && \
    apt-get install -yqq autoconf bison cmake curl file flex g++ git libnuma-dev libpq-dev libssl-dev \
                     libtool libyajl-dev make wget software-properties-common > /dev/null

### Install php

RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql  > /dev/null

COPY deploy/conf/* /etc/php/7.3/fpm/

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.3/fpm/php-fpm.conf ; fi;

### Install h2o

ENV H2O_VERSION=2.2.5
ENV H2O_ARCHIVE="v${H2O_VERSION}.tar.gz"
ENV H2O_HOME=/h2o

RUN wget -qO "$H2O_ARCHIVE" "https://github.com/h2o/h2o/archive/$H2O_ARCHIVE" && \
    tar xf "$H2O_ARCHIVE" && \
    cd "h2o-$H2O_VERSION" && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=off && \
    make -j "$(nproc)" install  > /dev/null

CMD service php7.3-fpm start && \
    /h2o/bin/h2o -c /deploy/h2o.conf