FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php8.4 php8.4-common php8.4-cli php8.4-fpm php8.4-mysql  > /dev/null

COPY deploy/conf/* /etc/php/8.4/fpm/

WORKDIR /flight
COPY . .

ENV FLIGHT_DIR="/flight/src"

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.4-dev libevent-dev php8.4-xml > /dev/null
RUN pecl install event-3.1.4 > /dev/null && echo "extension=event.so" > /etc/php/8.4/cli/conf.d/event.ini

RUN composer require joanhey/adapterman:^0.7
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN sed -i "s|Flight::start() ;|//Flight::start() ;|g" index.php

RUN chmod -R 777 /flight

EXPOSE 8080

CMD php -c deploy/conf/cli-php.ini \
    server.php start
