FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq 

RUN apt-get install -yqq git unzip curl \
    php8.2 php8.2-common php8.2-cli php8.2-fpm php8.2-mysql > /dev/null

COPY deploy/conf/* /etc/php/8.2/fpm/

# Install Caddyserver
RUN apt-get install -y debian-keyring debian-archive-keyring apt-transport-https > /dev/null \
    && curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg \
    && curl -sf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | tee /etc/apt/sources.list.d/caddy-stable.list \
    && apt-get update > /dev/null && apt-get install caddy > /dev/null

ADD ./ /php
WORKDIR /php

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.2/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /php

EXPOSE 8080

CMD service php8.2-fpm start && \
    caddy run --config deploy/caddy/Caddyfile
