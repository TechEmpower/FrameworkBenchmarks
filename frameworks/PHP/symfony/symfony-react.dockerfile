FROM php:8.4-cli

RUN apt-get update -yqq && \
    apt-get install -yqq libpq-dev libicu-dev git > /dev/null && \
    docker-php-ext-install pdo_pgsql opcache intl pcntl > /dev/null

COPY --link deploy/swoole/php.ini /usr/local/etc/php/
WORKDIR /symfony
COPY --link . .

# We deal with concurrencies over 1k, which stream_select doesn't support.
# libuv
RUN apt-get install -yqq libuv1-dev > /dev/null \
     && pecl install uv-beta > /dev/null
RUN docker-php-ext-enable uv

# libevent
# RUN apt-get install -y libevent-dev > /dev/null \
#    && pecl install event-3.1.4 > /dev/null
# RUN docker-php-ext-enable event


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
