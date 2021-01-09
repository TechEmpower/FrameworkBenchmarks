FROM php:8.0

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole
RUN docker-php-ext-install pdo_mysql pcntl opcache > /dev/null

RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit=1205" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

ADD ./ /lumen
WORKDIR /lumen

RUN mkdir -p /lumen/bootstrap/cache /lumen/storage/logs /lumen/storage/framework/sessions /lumen/storage/framework/views /lumen/storage/framework/cache
RUN chmod -R 777 /lumen

RUN deploy/swoole/install-composer.sh
RUN apt-get update > /dev/null && \
    apt-get install -yqq git unzip > /dev/null
COPY deploy/laravel-s/composer* ./

RUN echo "LARAVELS_LISTEN_IP=0.0.0.0" >> .env
RUN echo "LARAVELS_LISTEN_PORT=5200" >> .env

RUN php composer.phar install -a --no-dev --quiet
RUN php artisan laravels publish

EXPOSE 5200

CMD bin/laravels start
