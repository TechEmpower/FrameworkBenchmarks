FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip \
    php8.1-cli php8.1-mysql php8.1-mbstring php8.1-xml php8.1-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.1-dev libevent-dev > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.1/cli/conf.d/event.ini

ADD ./ /laravel
WORKDIR /laravel

EXPOSE 8080

RUN mkdir -p /laravel/bootstrap/cache /laravel/storage/logs /laravel/storage/framework/sessions /laravel/storage/framework/views /laravel/storage/framework/cache
RUN chmod -R 777 /laravel

COPY deploy/workerman/composer.json ./
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN php artisan optimize

COPY deploy/conf/cli-php.ini /etc/php/8.1/cli/php.ini

CMD php server-man.php start
