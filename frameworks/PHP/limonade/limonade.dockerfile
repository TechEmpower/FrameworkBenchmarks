FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql > /dev/null

RUN apt install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /limonade
WORKDIR /limonade

RUN composer install --quiet

CMD service php7.2-fpm start && \
    nginx -c /limonade/deploy/nginx.conf -g "daemon off;"
