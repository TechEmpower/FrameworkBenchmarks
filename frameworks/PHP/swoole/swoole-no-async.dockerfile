FROM php:7.4

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

ADD ./ /swoole
WORKDIR /swoole

COPY php.ini /usr/local/etc/php/

CMD php swoole-server-noasync.php
