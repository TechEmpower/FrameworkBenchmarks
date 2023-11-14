FROM php:8.2-cli

RUN apt-get update && apt-get install -y git > /dev/null

RUN docker-php-ext-install opcache pdo_mysql > /dev/null

RUN pecl install openswoole > /dev/null && \
    docker-php-ext-enable openswoole

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY php.ini /usr/local/etc/php/

ADD ./ /openswoole
WORKDIR /openswoole

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

CMD php /openswoole/openswoole-server-pdo.php
