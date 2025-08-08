FROM php:8.4-cli

RUN docker-php-ext-install pdo_mysql pcntl opcache sockets > /dev/null

RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit=1205" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

WORKDIR /laravel
COPY --link . .

RUN mkdir -p bootstrap/cache \
            storage/logs \
            storage/framework/sessions \
            storage/framework/views \
            storage/framework/cache

RUN apt-get update > /dev/null && \
    apt-get install -yqq curl unzip > /dev/null

RUN pecl install protobuf > /dev/null && echo "extension=protobuf.so" > /usr/local/etc/php/conf.d/protobuf.ini

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN composer require laravel/octane --update-no-dev --no-scripts --quiet
RUN php artisan octane:install --server="roadrunner"
RUN php artisan optimize

EXPOSE 8080

# https://artisan.page/12.x/
#ENTRYPOINT ["php", "artisan", "octane:roadrunner", "--host=0.0.0.0", "--port=8080", "--workers=auto", "--max-requests=10000", "--rr-config=/laravel/deploy/roadrunner/.rr.yaml"]
ENTRYPOINT ["/laravel/rr", "serve", "-c", "/laravel/deploy/roadrunner/.rr.yaml"]
