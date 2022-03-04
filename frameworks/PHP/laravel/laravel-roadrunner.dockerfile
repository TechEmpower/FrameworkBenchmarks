FROM php:8.0-cli

RUN docker-php-ext-install pdo_mysql pcntl opcache sockets > /dev/null

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

COPY deploy/roadrunner/composer.json ./
COPY deploy/roadrunner/.rr.yaml ./

RUN composer install -a --no-dev --quiet
RUN php artisan optimize

# `./vendor/bin/rr get-binary` is github rate-limited
RUN tar xzf deploy/roadrunner/roadrunner-*.tar.gz && mv roadrunner-*/rr . && chmod +x ./rr
RUN php artisan vendor:publish --provider='Spiral\RoadRunnerLaravel\ServiceProvider' --tag=config

EXPOSE 8080

# CMD bash
CMD ./rr serve -c ./.rr.yaml

