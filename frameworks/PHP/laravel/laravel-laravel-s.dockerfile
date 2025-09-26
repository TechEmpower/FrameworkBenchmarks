FROM php:8.0-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole
RUN docker-php-ext-install pdo_mysql pcntl opcache > /dev/null

RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
#RUN echo "opcache.jit=1205" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
#RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

ADD ./ /laravel
WORKDIR /laravel

RUN mkdir -p /laravel/bootstrap/cache /laravel/storage/logs /laravel/storage/framework/sessions /laravel/storage/framework/views /laravel/storage/framework/cache
RUN chmod -R 777 /laravel

RUN apt-get update > /dev/null && \
    apt-get install -yqq git unzip > /dev/null
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" && php composer-setup.php && php -r "unlink('composer-setup.php');"
RUN mv composer.phar /usr/local/bin/composer

COPY deploy/laravel-s/composer.json ./

RUN echo "LARAVELS_LISTEN_IP=0.0.0.0" >> .env
RUN echo "LARAVELS_LISTEN_PORT=8080" >> .env

RUN composer install -a --no-dev --quiet
RUN php artisan optimize
RUN php artisan laravels publish

EXPOSE 8080

CMD php bin/laravels start
