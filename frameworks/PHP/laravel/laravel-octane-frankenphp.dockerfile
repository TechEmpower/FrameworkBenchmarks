FROM dunglas/frankenphp
 
RUN install-php-extensions \
    pcntl \
    pdo_mysql \
	intl \
	zip \
	opcache > /dev/null
 
COPY . /app

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN mkdir -p /app/bootstrap/cache /app/storage/logs /app/storage/framework/sessions /app/storage/framework/views /app/storage/framework/cache
RUN chmod -R 777 /app

COPY deploy/conf/php.ini  /usr/local/etc/php

RUN composer require laravel/octane guzzlehttp/guzzle

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN php artisan optimize

RUN frankenphp -v

EXPOSE 8080

ENTRYPOINT ["php", "artisan", "octane:frankenphp", "--port=8080"]
