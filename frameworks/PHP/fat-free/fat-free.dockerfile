FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php7.4 php7.4-common php7.4-cli php7.4-fpm php7.4-mysql  > /dev/null

COPY deploy/conf/* /etc/php/7.4/fpm/

ADD ./ /fat-free
WORKDIR /fat-free

ENV F3DIR="/fat-free/src"

RUN git clone -b 3.6.5 --single-branch --depth 1 "https://github.com/bcosca/fatfree-core.git" src

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.4/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /fat-free

CMD service php7.4-fpm start && \
    nginx -c /fat-free/deploy/nginx.conf -g "daemon off;"
