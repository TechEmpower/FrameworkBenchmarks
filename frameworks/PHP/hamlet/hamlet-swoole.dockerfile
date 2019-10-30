FROM php:7.3

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install mysqli > /dev/null && \
    docker-php-ext-enable mysqli

RUN apt-get update > /dev/null && \
    apt-get install -y git unzip > /dev/null

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /php
WORKDIR /php
RUN chmod -R 777 /php

RUN composer require hamlet-framework/http-swoole:dev-master --quiet
RUN composer require hamlet-framework/db-mysql-swoole:dev-master --quiet
RUN composer update --no-dev --quiet

CMD php /php/swoole.php
