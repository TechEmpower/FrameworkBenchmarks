FROM ubuntu:22.04

ENV PHP_VERSION 8.2.10
ENV SWOOLE_VERSION 5.1.0
ENV ENABLE_COROUTINE 1
ENV DATABASE_DRIVER pgsql

RUN apt update > /dev/null \
    && apt install -y autoconf re2c bison gcc g++ libxml2-dev pkg-config libsqlite3-dev libssl-dev libpq-dev zlib1g-dev make curl > /dev/null \
    && cd /tmp \
    && curl -sSL "https://github.com/php/php-src/archive/refs/tags/php-${PHP_VERSION}.tar.gz" | tar xzf - > /dev/null \
    && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - > /dev/null \
    && ln -s /tmp/swoole-src-${SWOOLE_VERSION} /tmp/php-src-php-${PHP_VERSION}/ext/swoole > /dev/null \
    && cd /tmp/php-src-php-${PHP_VERSION}/  \
    && ./buildconf --force > /dev/null \
    && export C_INCLUDE_PATH=/tmp/php-src-php-${PHP_VERSION}/ext/:$C_INCLUDE_PATH \
    && ./configure --prefix=/tmp/php/ --enable-opcache --enable-swoole --enable-swoole-pgsql > /dev/null \
    && make -j2 > /dev/null \
    && make install > /dev/null \
    && ln -s /tmp/php/bin/php /usr/bin/php

COPY php.ini /tmp/php/lib/

WORKDIR /swoole

ADD ./swoole-server.php /swoole
ADD ./database.php /swoole
EXPOSE 8080
CMD php /swoole/swoole-server.php
