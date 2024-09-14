FROM php:8.3-cli

COPY --from=ghcr.io/roadrunner-server/roadrunner:2023.3 --link /usr/bin/rr /usr/local/bin/rr
COPY --from=mlocati/php-extension-installer --link /usr/bin/install-php-extensions /usr/local/bin/
COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN install-php-extensions \
    intl \
    opcache \
    pdo_pgsql \
    sockets \
    zip > /dev/null

COPY --link deploy/conf/php.ini /usr/local/etc/php/
WORKDIR /symfony
COPY --link . .

ENV APP_RUNTIME="Runtime\RoadRunnerSymfonyNyholm\Runtime"
RUN composer require runtime/roadrunner-symfony-nyholm --update-no-dev --no-scripts --quiet
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

EXPOSE 8080

RUN rr -v
ENTRYPOINT ["rr", "serve"]
