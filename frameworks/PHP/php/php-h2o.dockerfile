FROM ubuntu:19.10

COPY ./ ./

RUN apt update > /dev/null && \
    apt install -yqq autoconf bison cmake curl file flex g++ git libnuma-dev libpq-dev libssl-dev \
                     libtool libyajl-dev libz-dev make wget software-properties-common > /dev/null

### Install php
ENV DEBIAN_FRONTEND noninteractive

RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php7.4 php7.4-common php7.4-cli php7.4-fpm php7.4-mysql  > /dev/null

COPY deploy/conf/* /etc/php/7.4/fpm/

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.4/fpm/php-fpm.conf ; fi;

### Install h2o

ENV H2O_VERSION=2.2.6
ENV H2O_ARCHIVE="v${H2O_VERSION}.tar.gz"
ENV H2O_HOME=/h2o

RUN wget -qO "$H2O_ARCHIVE" "https://github.com/h2o/h2o/archive/$H2O_ARCHIVE" && \
    tar xf "$H2O_ARCHIVE" && \
    cd "h2o-$H2O_VERSION" && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib -DWITH_MRUBY=off . && \
    make -j "$(nproc)" install  > /dev/null

CMD export WORKERS=$(( 2 * $(nproc) )) && \
    sed -i "s/num-threads: x/num-threads: $WORKERS/g" /deploy/h2o.conf && \
    service php7.4-fpm start && \
    /h2o/bin/h2o -c /deploy/h2o.conf
