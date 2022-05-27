FROM php:8.1-rc-cli

RUN pecl install openswoole > /dev/null && \
    docker-php-ext-enable openswoole

RUN docker-php-ext-install opcache pdo_mysql > /dev/null

ADD ./ /openswoole
WORKDIR /openswoole

COPY php.ini /usr/local/etc/php/

EXPOSE 8080

CMD php swoole-server-noasync.php
