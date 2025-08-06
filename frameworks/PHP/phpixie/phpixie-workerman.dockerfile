FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php8.0 php8.0-common php8.0-cli php8.0-mysql php8.0-xml php8.0-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.0-dev libevent-dev > /dev/null
RUN pecl install event-3.1.4 > /dev/null && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini

COPY deploy/conf/adapterman.ini /etc/php/8.0/cli/conf.d/20-adapterman.ini

WORKDIR /phpixie
COPY --link . .

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN composer require joanhey/adapterman:^0.6 --quiet

RUN chmod -R 777 /phpixie

EXPOSE 8080
CMD php server.php start
