FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get update -yqq > /dev/null && apt-get install -yqq git \
    php8.2-cli php8.2-mysql php8.2-mbstring php8.2-xml php8.2-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.2-dev libevent-dev > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.2/cli/conf.d/event.ini

COPY deploy/conf/cli-php.ini /etc/php/8.2/cli/php.ini

ADD ./ /slim
WORKDIR /slim

EXPOSE 8080

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet 
RUN composer require joanhey/adapterman:^0.6 --quiet


RUN sed -i 's|$app->run(); //| //$app->run(); //|g' index.php
RUN sed -i 's|//global $app;|global $app;|g' index.php
RUN sed -i 's|//PDO::ATTR_EMULATE_PREPARES|PDO::ATTR_EMULATE_PREPARES|g' index.php

RUN chmod -R 777 /slim

CMD php server.php start
