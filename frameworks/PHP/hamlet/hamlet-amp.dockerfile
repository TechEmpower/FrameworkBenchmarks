FROM ubuntu:18.10

ENV PHP_VERSION="7.3"

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq software-properties-common > /dev/null

RUN add-apt-repository -y ppa:ondrej/php && \
    apt-get update -yqq > /dev/null && \
    apt-get install -yqq \
        curl \
        git \
        unzip \
        php${PHP_VERSION}-cli \
        php${PHP_VERSION}-mysqli \
        php${PHP_VERSION}-apcu \
        php${PHP_VERSION}-opcache > /dev/null

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /php
WORKDIR /php
RUN chmod -R 777 /php

RUN composer update --quiet --no-dev --optimize-autoloader --classmap-authoritative

CMD php /php/amp.php
