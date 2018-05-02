FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common apt-transport-https > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
RUN add-apt-repository https://dl.hhvm.com/ubuntu
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq hhvm nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql php7.2-xml php7.2-mbstring  > /dev/null

RUN apt-get install -yqq composer > /dev/null

ADD ./ /codeigniter
WORKDIR /codeigniter

RUN composer install --quiet

CMD hhvm -m daemon --config /codeigniter/deploy/config.hdf && \
    nginx -c /codeigniter/deploy/nginx-hhvm.conf -g "daemon off;"
