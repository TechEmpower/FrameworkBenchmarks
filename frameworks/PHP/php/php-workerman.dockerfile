FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq 

RUN apt-get install -yqq git unzip \
    php8.3 php8.3-common php8.3-cli php8.3-fpm php8.3-mysql  > /dev/null

RUN apt-get install -y php-pear php8.3-dev php8.3-xml libevent-dev > /dev/null
RUN pecl install event-3.1.3 > /dev/null && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/event.ini

COPY deploy/workerman/cli-php.ini /etc/php/8.3/cli/php.ini

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

ADD ./ /php
WORKDIR /php

COPY deploy/workerman/composer.json ./
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev 

COPY deploy/workerman/start.php ./

RUN chmod -R 777 /php

EXPOSE 8080

CMD php start.php start
