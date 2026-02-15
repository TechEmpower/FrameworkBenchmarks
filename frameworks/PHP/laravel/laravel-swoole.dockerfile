FROM phpswoole/swoole:php8.5

RUN apt-get update -yqq && apt-get install libicu-dev -y > /dev/null && \
    docker-php-ext-install intl pcntl > /dev/null
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

RUN composer require laravel/octane:^2 --update-no-dev --no-scripts --quiet
RUN php artisan octane:install --server="swoole"
RUN php artisan optimize

EXPOSE 8080

ENTRYPOINT ["php", "artisan", "octane:swoole", "--host=0.0.0.0", "--port=8080", "--workers=auto", "--task-workers=auto", "--max-requests=10000"]
