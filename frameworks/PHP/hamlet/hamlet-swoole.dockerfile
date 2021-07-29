FROM php:8.0

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install mysqli > /dev/null && \
    docker-php-ext-enable mysqli

RUN apt-get update -yqq && \
    apt-get install -yqq git unzip

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /php
WORKDIR /php
COPY ./composer-swoole.json composer.json
RUN chmod -R 777 /php

RUN composer update --no-dev --quiet

EXPOSE 8080

CMD php /php/swoole.php
