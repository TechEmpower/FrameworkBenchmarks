FROM php:8.1-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install opcache pdo_mysql > /dev/null

ADD ./ /swoole
WORKDIR /swoole

COPY php.ini /usr/local/etc/php/

EXPOSE 8080

CMD php swoole-server-noasync.php
