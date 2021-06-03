FROM php:7.4

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install mysqli > /dev/null && \
    docker-php-ext-enable mysqli

RUN docker-php-ext-install pdo_mysql > /dev/null && \
    docker-php-ext-enable pdo_mysql

RUN apt-get update -yqq && \
    apt-get install -yqq git unzip

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /php
WORKDIR /php
RUN chmod -R 777 /php

RUN composer require hamlet-framework/http-swoole:dev-master --quiet
RUN composer require hamlet-framework/db-mysql-swoole:dev-master --quiet
RUN composer update --no-dev --quiet

EXPOSE 8080

CMD php /php/swoole.php
