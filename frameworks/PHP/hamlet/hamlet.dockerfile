FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null
RUN apt-get install -yqq nginx git unzip curl php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql php7.3-opcache > /dev/null

COPY deploy/conf/* /etc/php/7.3/fpm/

ADD ./ /php
WORKDIR /php
RUN chmod -R 777 /php

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 2048|pm.max_children = 512|g" /etc/php/7.3/fpm/php-fpm.conf ; fi;

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer update --no-dev --optimize-autoloader --classmap-authoritative

CMD service php7.3-fpm start && \
    nginx -c /php/deploy/nginx-fpm.conf -g "daemon off;"
