FROM php:7.4

RUN docker-php-ext-install opcache  > /dev/null

ENV SWOOLE_VERSION=4.5.1

RUN     apt-get update && apt-get install -y libpq-dev \
        && cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole

RUN     cd /tmp && curl -sSL "https://github.com/swoole/ext-postgresql/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd ext-postgresql-${SWOOLE_VERSION} \
        && phpize && ./configure > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole_postgresql
 
WORKDIR /swoole

COPY swoole-server.php swoole-server.php
RUN sed -i "s|_postgres||g" swoole-server.php

COPY php.ini /usr/local/etc/php/

CMD php swoole-server.php
