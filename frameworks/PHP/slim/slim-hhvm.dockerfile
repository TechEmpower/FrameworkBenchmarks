FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common apt-transport-https
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
RUN add-apt-repository https://dl.hhvm.com/ubuntu
RUN apt update -yqq
RUN apt install -yqq hhvm nginx git unzip php5.6 php5.6-common php5.6-cli php5.6-fpm php5.6-mysql php5.6-xml php5.6-mbstring php5.6-mcrypt

RUN apt install -yqq composer

ADD ./ /slim
WORKDIR /slim

RUN composer install --quiet

RUN chmod -R 777 /slim

CMD hhvm -m daemon --config /slim/deploy/config.hdf && \
    nginx -c /slim/deploy/nginx-hhvm.conf -g "daemon off;"
