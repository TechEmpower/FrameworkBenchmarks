FROM ubuntu:20.04

ENV PHP_VERSION 8.0
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip curl \
    php${PHP_VERSION}-cli php${PHP_VERSION}-fpm php${PHP_VERSION}-apcu php${PHP_VERSION}-pdo-mysql > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

ADD ./ /app
WORKDIR /app

RUN composer update --no-dev --quiet

COPY deploy/fpm/php-fpm.conf /etc/php/${PHP_VERSION}/fpm/php-fpm.conf
COPY deploy/fpm/php.ini /etc/php/${PHP_VERSION}/fpm/php.ini

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/${PHP_VERSION}/fpm/php-fpm.conf ; fi;

EXPOSE 8080

CMD service php${PHP_VERSION}-fpm start \
    && nginx -c /app/deploy/fpm/nginx.conf -g "daemon off;"
