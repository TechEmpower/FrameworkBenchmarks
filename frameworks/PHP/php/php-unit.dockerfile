FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
#RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq curl php-mysql > /dev/null

RUN curl https://nginx.org/keys/nginx_signing.key | apt-key add - \
    && add-apt-repository "deb https://packages.nginx.org/unit/ubuntu/ focal unit" -s \
    && apt-get -y update \
    && apt-get -y install unit unit-php

ADD ./ /php
WORKDIR /php

# forward log to docker log collector
#RUN ln -sf /dev/stdout /var/log/unit.log

# RUN if [ $(nproc) = 2 ]; then sed -i "s|\"processes\": 128,|\"processes\": 64,|g" /php/deploy/nginx-unit.json ; fi;

RUN unitd && \
    curl -X PUT --data-binary @/php/deploy/nginx-unit.json --unix-socket \
        /var/run/control.unit.sock http://localhost/config

EXPOSE 8080

CMD unitd --no-daemon
