FROM php:8.4-cli

RUN apt-get update -yqq && \
    apt-get install -yqq libpq-dev libicu-dev git > /dev/null && \
    docker-php-ext-install pdo_pgsql opcache intl pcntl > /dev/null

COPY --link deploy/swoole/php.ini /usr/local/etc/php/
WORKDIR /symfony
COPY --link . .

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

#ENV APP_DEBUG 1
ENV APP_ENV prod

#ENV APP_RUNTIME "Runtime\React\Runtime"
#RUN composer require runtime/react --update-no-dev --no-scripts --quiet

ENV APP_RUNTIME "Zolex\ReactPhpBundle\Runtime\ReactPhpRuntime"
ENV REACT_HOST "0.0.0.0"
RUN composer require zolex/reactphp-bundle --update-no-dev --no-scripts --quiet
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

EXPOSE 8080

ENTRYPOINT [ "php", "/symfony/public/runtime.php" ]
