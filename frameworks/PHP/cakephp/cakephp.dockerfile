FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php5.6 php5.6-common php5.6-cli php5.6-fpm php5.6-mysql php5.6-xml php5.6-mbstring php5.6-mcrypt

RUN apt install -yqq composer

COPY deploy/conf/* /etc/php/5.6/fpm/
RUN sed -i "s|listen = /run/php/php7.2-fpm.sock|listen = /run/php/php5.6-fpm.sock|g" /etc/php/5.6/fpm/php-fpm.conf

ADD ./ /cakephp
WORKDIR /cakephp

RUN mkdir -p app/tmp/cache/models
RUN mkdir -p app/tmp/cache/persistent
RUN mkdir -p app/tmp/logs
RUN chmod -R 777 app/tmp

RUN composer install --quiet

CMD service php5.6-fpm start && \
    nginx -c /cakephp/deploy/nginx.conf -g "daemon off;"
