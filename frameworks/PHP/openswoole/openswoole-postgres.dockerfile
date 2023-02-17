FROM php:8.1-cli

RUN docker-php-ext-install opcache  > /dev/null

ENV SWOOLE_VERSION 4.8.0

WORKDIR /tmp

RUN     apt-get update && apt-get install -y libpq-dev \
        && curl -sSL "https://github.com/openswoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar --strip-components=1 -xz \
        && phpize && ./configure --with-postgres > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable openswoole

WORKDIR /openswoole

COPY swoole-server.php swoole-server.php
RUN sed -i "s|_postgres||g" swoole-server.php

COPY php.ini /usr/local/etc/php/

EXPOSE 8080

CMD php swoole-server.php
