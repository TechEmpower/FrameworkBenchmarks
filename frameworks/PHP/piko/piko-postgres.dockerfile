FROM ubuntu:24.04

ENV DATABASE_DRIVER pgsql

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get install -yqq nginx git unzip php8.4-fpm php8.4-pgsql > /dev/null

COPY deploy/conf/* /etc/php/8.4/fpm/

WORKDIR /piko
COPY --link . .

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.4/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /piko

EXPOSE 8080

CMD set -e; \
    /usr/sbin/nginx -c /piko/deploy/nginx.conf 2>&1 > /dev/stderr & \
    exec /usr/sbin/php-fpm8.4 --nodaemonize --fpm-config /etc/php/8.4/fpm/php-fpm.conf
