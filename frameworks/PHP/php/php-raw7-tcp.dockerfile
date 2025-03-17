FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php8.4 php8.4-common php8.4-cli php8.4-fpm php8.4-mysql  > /dev/null

COPY deploy/conf/* /etc/php/8.4/fpm/

ADD ./ /php
WORKDIR /php

RUN sed -i "s|listen = /run/php/php-fpm.sock|listen = 127.0.0.1:9001|g" /etc/php/8.4/fpm/php-fpm.conf
RUN sed -i "s|server unix:/var/run/php/php-fpm.sock;|server 127.0.0.1:9001;|g" deploy/nginx7.conf

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.4/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /php

EXPOSE 8080

CMD service php8.4-fpm start && \
    nginx -c /php/deploy/nginx7.conf -g "daemon off;"
