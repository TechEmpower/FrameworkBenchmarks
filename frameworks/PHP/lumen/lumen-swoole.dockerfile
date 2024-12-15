FROM phpswoole/swoole:5.1.3-php8.3

RUN docker-php-ext-install pcntl opcache curl > /dev/null

WORKDIR /lumen
COPY --link . .

COPY --link deploy/swoole/php.ini /usr/local/etc/php/

RUN mkdir -p /lumen/storage/framework/sessions /lumen/storage/framework/views /lumen/storage/framework/cache

COPY deploy/swoole/composer* ./
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN echo "APP_SWOOLE=true" >> .env

RUN chmod -R 777 /lumen

EXPOSE 8080

ENTRYPOINT [ "php", "artisan", "swoole:http", "start" ]
