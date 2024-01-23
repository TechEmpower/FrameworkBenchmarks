FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php8.3 php8.3-common php8.3-cli php8.3-fpm php8.3-mysql php8.3-dev > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.3/fpm/

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.3/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/app/cache/*

COPY deploy/conf/ubiquity-config.php app/config/config.php

RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php" >> /etc/php/8.3/fpm/php.ini

EXPOSE 8080

CMD service php8.3-fpm start && \
    nginx -c /ubiquity/deploy/nginx.conf -g "daemon off;"
