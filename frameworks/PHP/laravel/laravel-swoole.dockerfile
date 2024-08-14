FROM phpswoole/swoole:5.1.3-php8.3

RUN docker-php-ext-install pcntl opcache curl > /dev/null

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

COPY --link deploy/swoole/composer.json .

RUN echo "APP_SWOOLE=true" >> .env

RUN composer install -a --no-dev --quiet
RUN php artisan optimize

EXPOSE 8080

ENTRYPOINT [ "php", "artisan", "swoole:http", "start" ]
