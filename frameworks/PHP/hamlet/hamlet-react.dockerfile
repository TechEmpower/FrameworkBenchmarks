FROM php:7.3-rc-cli

RUN docker-php-ext-install mysqli > /dev/null && \
    docker-php-ext-enable mysqli

RUN docker-php-ext-install opcache > /dev/null && \
    docker-php-ext-enable opcache

RUN pecl install apcu > /dev/null && \
    docker-php-ext-enable apcu

RUN apt-get update > /dev/null && \
    apt-get install -y git unzip > /dev/null

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /php
WORKDIR /php
RUN chmod -R 777 /php

RUN composer update --quiet --no-dev --optimize-autoloader --classmap-authoritative

CMD php /php/react.php
