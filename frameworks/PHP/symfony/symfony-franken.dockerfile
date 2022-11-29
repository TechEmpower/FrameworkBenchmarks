FROM dunglas/frankenphp

# add additional extensions here:
RUN install-php-extensions \
    pdo_mysql \
    intl \
    opcache

RUN apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null && \
    apt-get install unzip > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

EXPOSE 8080

COPY deploy/Caddyfile /etc/Caddyfile

ADD . /symfony
WORKDIR /symfony

RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log
RUN composer install --no-dev --no-scripts --quiet

RUN composer require runtime/frankenphp-symfony
ENV FRANKENPHP_CONFIG="worker ./public/worker.php"
ENV APP_RUNTIME=Runtime\\FrankenPhpSymfony\\Runtime

RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --no-dev --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env prod

#ENV CADDY_DEBUG=debug
