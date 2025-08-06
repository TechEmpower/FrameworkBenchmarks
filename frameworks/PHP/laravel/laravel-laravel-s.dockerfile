FROM phpswoole/swoole:php8.0

RUN docker-php-ext-install pcntl curl > /dev/null

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

COPY --link deploy/laravel-s/composer.json .

RUN echo "LARAVELS_LISTEN_IP=0.0.0.0" >> .env
RUN echo "LARAVELS_LISTEN_PORT=8080" >> .env

RUN composer install -a --no-dev --quiet
RUN php artisan optimize
RUN php artisan laravels publish

EXPOSE 8080

ENTRYPOINT [ "php", "bin/laravels", "start" ]
