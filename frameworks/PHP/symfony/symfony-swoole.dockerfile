FROM php:8.3-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN pecl install apcu > /dev/null && \
    docker-php-ext-enable apcu

RUN apt-get update -yqq && \
    apt-get install -yqq libpq-dev libicu-dev git unzip > /dev/null && \ 
    docker-php-ext-install pdo_pgsql opcache intl > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

COPY --link deploy/swoole/php.ini /usr/local/etc/php/
WORKDIR /symfony
COPY --link . .

#ENV APP_DEBUG=1
ENV APP_RUNTIME="Runtime\Swoole\Runtime"
RUN composer require runtime/swoole --update-no-dev --no-scripts --quiet
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

EXPOSE 8080

ENTRYPOINT [ "php", "/symfony/public/swoole.php" ]
