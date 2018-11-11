FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq nginx git unzip php5.6 php5.6-common php5.6-cli php5.6-fpm php5.6-mysql  > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/5.6/fpm/
RUN sed -i "s|listen = /run/php/php7.2-fpm.sock|listen = /run/php/php5.6-fpm.sock|g" /etc/php/5.6/fpm/php-fpm.conf

ADD ./ /php
WORKDIR /php

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 2048|pm.max_children = 512|g" /etc/php/5.6/fpm/php-fpm.conf ; fi;

RUN composer install --quiet

RUN chmod -R 777 /php

CMD service php5.6-fpm start && \
    nginx -c /php/deploy/nginx5.conf -g "daemon off;"
