FROM ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql php7.3-mongodb  > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /phalcon
WORKDIR /phalcon

RUN apt-get install -yqq php7.3-phalcon  > /dev/null

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.2/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod -R 777 app

CMD service php7.3-fpm start && \
    nginx -c /phalcon/deploy/nginx.conf -g "daemon off;"
