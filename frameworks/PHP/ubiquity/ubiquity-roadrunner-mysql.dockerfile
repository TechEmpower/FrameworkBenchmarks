FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php8.0 php8.0-common php8.0-cgi php-curl php8.0-mysql > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php-dev > /dev/null

COPY deploy/conf/php-async.ini /etc/php/8.0/cgi/php.ini

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN chmod -R 777 /ubiquity

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

RUN composer require lapinskas/roadrunner-ubiquity:1.1.1 --quiet

RUN vendor/bin/rr get

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

COPY deploy/roadrunner/envwrapper.sh /bin/
RUN ln -s /ubiquity/.ubiquity/.rr.yml /ubiquity/.rr.yml
RUN chmod 755 /bin/envwrapper.sh

RUN chmod 777 -R /ubiquity/.ubiquity/*

#RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php" >> /etc/php/8.0/cgi/php.ini
RUN echo "opcache.jit_buffer_size=128M\nopcache.jit=tracing\n" >> /etc/php/8.0/cgi/php.ini

COPY deploy/conf/roadrunner/mysql/rrServices.php app/config/rrServices.php

EXPOSE 8080

CMD envwrapper.sh /ubiquity/rr serve
