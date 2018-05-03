FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql

RUN apt install -yqq composer

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /zend1
WORKDIR /zend1

RUN composer install --quiet

CMD service php7.2-fpm start && \
    nginx -c /zend1/deploy/nginx.conf -g "daemon off;"
