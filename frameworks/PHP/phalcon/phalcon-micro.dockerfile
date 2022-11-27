FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.1-cli php8.1-fpm php8.1-mysql php8.1-mbstring php8.1-xml > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.1/fpm/

ADD ./ /phalcon
WORKDIR /phalcon

RUN apt-get install -y php-pear php8.1-dev > /dev/null
RUN pecl install phalcon > /dev/null && echo "extension=phalcon.so" > /etc/php/8.1/fpm/conf.d/phalcon.ini

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.1/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --ignore-platform-reqs

RUN mv /phalcon/public/index-micro.php /phalcon/public/index.php

RUN chmod -R 777 app

EXPOSE 8080

CMD service php8.1-fpm start && \
    nginx -c /phalcon/deploy/nginx.conf
