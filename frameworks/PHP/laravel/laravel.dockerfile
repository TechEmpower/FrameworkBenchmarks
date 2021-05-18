FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip \
    php8.0-cli php8.0-fpm php8.0-mysql  > /dev/null
RUN apt-get install -yqq php8.0-mbstring php8.0-xml  > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/8.0/fpm/

ADD ./ /laravel
WORKDIR /laravel

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.0/fpm/php-fpm.conf ; fi;

RUN mkdir -p /laravel/bootstrap/cache /laravel/storage/logs /laravel/storage/framework/sessions /laravel/storage/framework/views /laravel/storage/framework/cache
RUN chmod -R 777 /laravel

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN php artisan optimize

EXPOSE 8080

# Uncomment next line for Laravel console error logging to be viewable in docker logs
# RUN echo "catch_workers_output = yes" >> /etc/php/8.0/fpm/php-fpm.conf

RUN mkdir -p /run/php
CMD /usr/sbin/php-fpm8.0 --fpm-config /etc/php/8.0/fpm/php-fpm.conf && nginx -c /laravel/deploy/nginx.conf

