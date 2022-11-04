FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.1-cli php8.1-fpm php8.1-mysql  php8.1-mbstring php8.1-xml php8.1-dev > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.1/fpm/

ADD ./ /laravel
WORKDIR /laravel

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.1/fpm/php-fpm.conf ; fi;

RUN mkdir -p /laravel/bootstrap/cache /laravel/storage/logs /laravel/storage/framework/sessions /laravel/storage/framework/views /laravel/storage/framework/cache
RUN chmod -R 777 /laravel

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN php artisan optimize

EXPOSE 8080

# Uncomment next line for Laravel console error logging to be viewable in docker logs
# RUN echo "catch_workers_output = yes" >> /etc/php/8.1/fpm/php-fpm.conf

RUN mkdir -p /run/php
CMD /usr/sbin/php-fpm8.1 --fpm-config /etc/php/8.1/fpm/php-fpm.conf && \
    nginx -c /laravel/deploy/nginx.conf
