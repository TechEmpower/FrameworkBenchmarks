FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql  > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /fat-free
WORKDIR /fat-free

ENV F3DIR="/fat-free/src"

RUN git clone "https://github.com/bcosca/fatfree-core.git" src
RUN cd src && git checkout -q "069ccd84afd2461c7ebb67f660c142f97577e661" # v3.5.2-dev

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 2048|pm.max_children = 512|g" /etc/php/7.2/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /fat-free

CMD service php7.2-fpm start && \
    nginx -c /fat-free/deploy/nginx.conf -g "daemon off;"
