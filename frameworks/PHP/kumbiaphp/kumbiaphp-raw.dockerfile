FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /kumbiaphp
WORKDIR /kumbiaphp

RUN git clone -b v1.0.0-rc.2 --single-branch --depth 1 -q https://github.com/KumbiaPHP/KumbiaPHP.git vendor/Kumbia

CMD service php7.2-fpm start && \
    nginx -c /kumbiaphp/deploy/nginx.conf -g "daemon off;"
