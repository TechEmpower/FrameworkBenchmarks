FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql
RUN apt install -yqq php7.2-mbstring php7.2-xml

RUN apt install -yqq composer

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /lumen
WORKDIR /lumen

RUN composer install --quiet

RUN mkdir -p /lumen/storage
RUN mkdir -p /lumen/storage/framework/sessions
RUN mkdir -p /lumen/storage/framework/views
RUN mkdir -p /lumen/storage/framework/cache

RUN chmod -R 777 /lumen

CMD service php7.2-fpm start && \
    nginx -c /lumen/deploy/nginx.conf -g "daemon off;"
