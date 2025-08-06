FROM dunglas/frankenphp:php8.4

# add additional extensions here:
RUN install-php-extensions \
    opcache \
    pdo_pgsql \
    zip > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

COPY --link deploy/Caddyfile /etc/frankenphp/Caddyfile
COPY --link deploy/conf/php.ini /usr/local/etc/php/

WORKDIR /symfony
COPY --link . .

ENV FRANKENPHP_CONFIG="worker /symfony/public/runtime.php"
ENV APP_RUNTIME="Runtime\FrankenPhpSymfony\Runtime"
#ENV CADDY_DEBUG=debug
RUN composer require runtime/frankenphp-symfony --update-no-dev --no-scripts --quiet
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

EXPOSE 8080

RUN frankenphp -v
