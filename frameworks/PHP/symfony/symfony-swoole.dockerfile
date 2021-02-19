FROM php:8.0

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN pecl install apcu > /dev/null && \
    docker-php-ext-enable apcu

RUN apt-get update -yqq && \
    apt-get install -yqq libicu-dev git unzip > /dev/null && \ 
    docker-php-ext-install pdo_mysql opcache intl > /dev/null

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

COPY deploy/swoole/php.ini /usr/local/etc/php/
WORKDIR /symfony
ADD ./composer.json /symfony/
RUN mkdir -m 777 -p /symfony/var/cache/swoole /symfony/var/log
RUN COMPOSER_ALLOW_SUPERUSER=1 composer install --no-dev --no-scripts --quiet
ADD . /symfony
RUN COMPOSER_ALLOW_SUPERUSER=1 composer require "k911/swoole-bundle:^0.9" --no-scripts --ignore-platform-reqs --quiet
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --no-dev --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env swoole

# removes hardcoded option `ATTR_STATEMENT_CLASS` conflicting with `ATTR_PERSISTENT`. Hack not needed when upgrading to Doctrine 3
# see https://github.com/doctrine/dbal/issues/2315
RUN sed -i '/PDO::ATTR_STATEMENT_CLASS/d' ./vendor/doctrine/dbal/lib/Doctrine/DBAL/Driver/PDOConnection.php

# Force debug=0 because env is not "prod"
ENV APP_DEBUG=0

RUN php bin/console cache:clear
RUN echo "opcache.preload=/symfony/var/cache/swoole/App_KernelSwooleContainer.preload.php" >> /usr/local/etc/php/php.ini

EXPOSE 8080

USER www-data
CMD php bin/console swoole:server:run
