FROM dunglas/frankenphp

# add additional extensions here:
RUN install-php-extensions \
    pdo_pgsql \
    intl \
    opcache

RUN apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null && \
    apt-get install unzip > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

EXPOSE 8080

COPY deploy/Caddyfile /etc/caddy/Caddyfile

ADD . /symfony
WORKDIR /symfony

RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log

ENV COMPOSER_ALLOW_SUPERUSER=1
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet --no-scripts
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

RUN composer require runtime/frankenphp-symfony
ENV FRANKENPHP_CONFIG="worker ./public/worker.php"
ENV APP_RUNTIME=Runtime\\FrankenPhpSymfony\\Runtime

#ENV CADDY_DEBUG=debug
