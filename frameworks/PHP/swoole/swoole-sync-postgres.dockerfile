FROM ubuntu:22.04

ENV SWOOLE_VERSION 5.1.1
ENV ENABLE_COROUTINE 0
ENV DATABASE_DRIVER pgsql

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq > /dev/null \
    && apt install -yqq software-properties-common > /dev/null \
    && LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null \
    && apt update -yqq > /dev/null \
    && apt install php8.3-cli php8.3-pdo-pgsql php8.3-dev libpq-dev -y > /dev/null \
    && cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
    && cd /tmp/swoole-src-${SWOOLE_VERSION} \
    && phpize > /dev/null \
    && ./configure > /dev/null \
    && make -j2 > /dev/null \
    && make install > /dev/null \
    && echo "extension=swoole.so" > /etc/php/8.3/cli/conf.d/50-swoole.ini

WORKDIR /swoole

ADD ./swoole-server.php /swoole
ADD ./php.ini /swoole
ADD ./database.php /swoole

RUN cat /swoole/php.ini >> /etc/php/8.3/cli/php.ini

EXPOSE 8080
CMD php /swoole/swoole-server.php
