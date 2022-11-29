FROM php:8.2-rc

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN pecl install apcu > /dev/null && \
    docker-php-ext-enable apcu

RUN apt-get update -yqq && \
    apt-get install -yqq libicu-dev git unzip > /dev/null && \ 
    docker-php-ext-install pdo_mysql opcache intl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/swoole/php.ini /usr/local/etc/php/

ADD . /symfony
WORKDIR /symfony
RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log
#RUN mkdir -m 777 -p /symfony/var/cache/swoole /symfony/var/log
RUN COMPOSER_ALLOW_SUPERUSER=1 composer install --no-dev --no-scripts --quiet

ENV APP_RUNTIME=Runtime\\Swoole\\Runtime
RUN composer require runtime/swoole

RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --no-dev --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env prod

#ENV APP_DEBUG=1

EXPOSE 8080

CMD php /symfony/public/swoole.php
