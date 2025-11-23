FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq
RUN apt-get install -yqq nginx git unzip \
    php8.5 php8.5-common php8.5-cli php8.5-fpm php8.5-mysql  > /dev/null

COPY deploy/conf/* /etc/php/8.5/fpm/

ADD ./ /php
WORKDIR /php

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.5/fpm/php-fpm.conf ; fi;
RUN sed -i "s|opcache.jit=off|;opcache.jit=off|g" /etc/php/8.5/fpm/conf.d/10-opcache.ini

RUN chmod -R 777 /php

EXPOSE 8080

CMD service php8.5-fpm start && \
    nginx -c /php/deploy/nginx7.conf -g "daemon off;"
