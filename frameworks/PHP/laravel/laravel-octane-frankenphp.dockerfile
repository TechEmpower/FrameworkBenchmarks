FROM dunglas/frankenphp:php8.5

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install libicu-dev unzip -y > /dev/null
RUN install-php-extensions \
    intl \
	pcntl \
    pdo_mysql \
	zip > /dev/null

COPY --link . /app/
COPY --from=composer --link /usr/bin/composer /usr/local/bin/composer

RUN mkdir -p bootstrap/cache \
            storage/logs \
            storage/framework/sessions \
            storage/framework/views \
            storage/framework/cache

COPY --link deploy/franken/php.ini  /usr/local/etc/php

RUN composer require laravel/octane guzzlehttp/guzzle --update-no-dev --no-scripts --quiet
RUN php artisan optimize

RUN frankenphp -v

EXPOSE 8080

ENTRYPOINT ["php", "artisan", "octane:frankenphp", "--port=8080", "--caddyfile=/app/deploy/franken/Caddyfile"]
