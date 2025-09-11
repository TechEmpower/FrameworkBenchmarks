FROM phpswoole/swoole:php8.4

RUN docker-php-ext-install pcntl opcache curl > /dev/null

RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit=1205" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

WORKDIR /hypervel
COPY --link . .

RUN mkdir -p bootstrap/cache \
            storage/logs \
            storage/framework/sessions \
            storage/framework/views \
            storage/framework/cache

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 9501

ENTRYPOINT ["php", "artisan", "serve"]
