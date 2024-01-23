FROM ubuntu:19.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common apt-transport-https > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
RUN add-apt-repository https://dl.hhvm.com/ubuntu
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq hhvm nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql php7.3-xml php7.3-mbstring php7.3-mongodb  > /dev/null

ADD ./ /hhvm_app
WORKDIR /hhvm_app

EXPOSE 8080

CMD hhvm -m daemon --config /hhvm_app/deploy/config.hdf && \
    nginx -c /hhvm_app/deploy/nginx.conf -g "daemon off;"
