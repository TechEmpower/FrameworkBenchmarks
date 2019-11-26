FROM php:7.3

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN pecl install apcu > /dev/null && \
    docker-php-ext-enable apcu

RUN docker-php-ext-install pdo pdo_mysql opcache  > /dev/null

RUN apt-get update -yqq && \
    apt-get install -yqq git unzip

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

COPY deploy/swoole/php.ini /usr/local/etc/php/
WORKDIR /symfony
ADD ./composer.json ./composer.json /symfony/
RUN mkdir -m 777 -p /symfony/var/cache/{dev,swoole} /symfony/var/log
RUN COMPOSER_ALLOW_SUPERUSER=1 composer install --no-dev
RUN COMPOSER_ALLOW_SUPERUSER=1 composer require k911/swoole-bundle --no-scripts
ADD . /symfony
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env swoole

# removes hardcoded option `ATTR_STATEMENT_CLASS` conflicting with `ATTR_PERSISTENT`. Hack not needed when upgrading to Doctrine 3
# see https://github.com/doctrine/dbal/issues/2315
RUN sed -i '/PDO::ATTR_STATEMENT_CLASS/d' ./vendor/doctrine/dbal/lib/Doctrine/DBAL/Driver/PDOConnection.php

RUN php bin/console cache:clear

ENV APP_DEBUG=0 \
    APP_ENV=swoole

CMD php bin/console swoole:server:run
