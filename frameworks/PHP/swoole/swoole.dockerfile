FROM php:8.1-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install opcache  > /dev/null

WORKDIR /swoole

COPY swoole-server.php swoole-server.php
RUN sed -i "s|DatabasePool('postgres|DatabasePool('mysql|g" swoole-server.php
RUN sed -i "s|_mysql||g" swoole-server.php

COPY php.ini /usr/local/etc/php/

EXPOSE 8080

CMD php swoole-server.php
