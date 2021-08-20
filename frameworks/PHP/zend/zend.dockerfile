FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php7.4 php7.4-common php7.4-cli php7.4-fpm php7.4-mysql  > /dev/null
RUN apt-get install -yqq php7.4-xml php7.4-mbstring  > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/7.4/fpm/

ADD ./ /zend
WORKDIR /zend

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.4/fpm/php-fpm.conf ; fi;

RUN mkdir -p data/cache
RUN chmod 777 data/cache

RUN composer install --optimize-autoloader --classmap-authoritative --quiet --no-dev

EXPOSE 8080

CMD service php7.4-fpm start && \
    nginx -c /zend/deploy/nginx.conf -g "daemon off;"
