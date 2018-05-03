FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql  > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /phalcon
WORKDIR /phalcon

RUN apt install -yqq php7.2-phalcon php7.2-dev  > /dev/null

RUN mv /phalcon/public/index-micro.php /phalcon/public/index.php

RUN chmod -R 777 app

CMD service php7.2-fpm start && \
    nginx -c /phalcon/deploy/nginx.conf -g "daemon off;"
