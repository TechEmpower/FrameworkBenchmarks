FROM dunglas/frankenphp

RUN install-php-extensions \
    @composer \
    pdo_mysql \
	zip \
	opcache > /dev/null

COPY . /app

RUN mkdir -p /app/bootstrap/cache /app/storage/logs /app/storage/framework/sessions /app/storage/framework/views /app/storage/framework/cache
RUN chmod -R 777 /app

COPY deploy/conf/php.ini  /usr/local/etc/php

RUN composer require laravel/octane

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN php artisan octane:install --server=frankenphp

RUN php artisan optimize

EXPOSE 8080

# start the FrankenPHP server directly (alteratively: php artisan octane:frankenphp)
ENTRYPOINT ["frankenphp", "run", "-c", "/app/deploy/frankenphp/Caddyfile"]
