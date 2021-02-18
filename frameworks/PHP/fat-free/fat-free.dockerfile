FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php8.0 php8.0-common php8.0-cli php8.0-fpm php8.0-mysql  > /dev/null

COPY deploy/conf/* /etc/php/8.0/fpm/

ADD ./ /fat-free
WORKDIR /fat-free

ENV F3DIR="/fat-free/src"

#RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer 
RUN apt-get install -yqq composer > /dev/null

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
#RUN git clone -b 3.7.2 --single-branch --depth 1 "https://github.com/bcosca/fatfree-core.git" src

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.0/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /fat-free

EXPOSE 8080

CMD service php8.0-fpm start && \
    nginx -c /fat-free/deploy/nginx.conf
