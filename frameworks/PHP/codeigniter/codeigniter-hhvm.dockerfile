FROM ubuntu:18.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common apt-transport-https > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xB4112585D386EB94
RUN add-apt-repository https://dl.hhvm.com/ubuntu
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq hhvm nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql php7.3-xml php7.3-mbstring  > /dev/null

RUN apt-get install -yqq composer > /dev/null

ADD ./ /codeigniter
WORKDIR /codeigniter

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

CMD hhvm -m daemon --config /codeigniter/deploy/config.hdf && \
    nginx -c /codeigniter/deploy/nginx-hhvm.conf -g "daemon off;"
