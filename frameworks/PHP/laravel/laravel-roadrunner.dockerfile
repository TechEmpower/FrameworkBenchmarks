FROM php:8.3-cli

RUN docker-php-ext-install pdo_mysql pcntl opcache sockets > /dev/null

RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
#RUN echo "opcache.jit=1205" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
#RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

WORKDIR /laravel
COPY --link . .

RUN mkdir -p /laravel/bootstrap/cache /laravel/storage/logs /laravel/storage/framework/sessions /laravel/storage/framework/views /laravel/storage/framework/cache

RUN apt-get update > /dev/null && \
    apt-get install -yqq curl unzip > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

COPY --link deploy/roadrunner/composer.json .
COPY --link deploy/roadrunner/.rr.yaml .

RUN composer install -a --no-dev --quiet
RUN php artisan optimize

# install roadrunner
COPY --from=ghcr.io/roadrunner-server/roadrunner:2023.3 --link /usr/bin/rr /usr/local/bin/rr

RUN php artisan vendor:publish --provider='Spiral\RoadRunnerLaravel\ServiceProvider' --tag=config
RUN rr -v

EXPOSE 8080

# CMD bash
CMD rr serve -c .rr.yaml
