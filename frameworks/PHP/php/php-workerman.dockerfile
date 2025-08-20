FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq

RUN apt-get install -yqq git unzip \
    php8.4 php8.4-common php8.4-cli php8.4-fpm php8.4-mysql  > /dev/null

RUN apt-get install -y php-pear php8.4-dev php8.4-xml libevent-dev > /dev/null
RUN pecl install event-3.1.4 > /dev/null && echo "extension=event.so" > /etc/php/8.4/cli/conf.d/event.ini

COPY deploy/workerman/cli-php.ini  /etc/php/8.4/cli/conf.d/20-adapterman.ini

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

WORKDIR /php
COPY --link . .

COPY deploy/workerman/composer.json ./
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev

COPY deploy/workerman/start.php ./

RUN chmod -R 777 /php

EXPOSE 8080

CMD php start.php start
