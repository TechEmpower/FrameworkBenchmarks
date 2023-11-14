FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git php8.1-cli php8.1-mongodb php8.1-xml php8.1-mbstring > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.1-dev libevent-dev > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.1/cli/conf.d/event.ini

COPY deploy/conf/php-async.ini /etc/php/8.1/cli/php.ini

ADD ./ /ubiquity

WORKDIR /ubiquity

RUN chmod -R 777 /ubiquity

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

RUN composer require phpmv/ubiquity:dev-nosql-prepare phpmv/ubiquity-devtools:dev-master phpmv/ubiquity-workerman:dev-master phpmv/ubiquity-nosql:dev-master --quiet

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/.ubiquity/*

COPY deploy/conf/workerman/mongo/workerServices.php app/config/workerServices.php

RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php" >> /etc/php/8.1/cli/php.ini
RUN echo "opcache.jit_buffer_size=128M\nopcache.jit=tracing\n" >> /etc/php/8.1/cli/php.ini

EXPOSE 8080

CMD /ubiquity/vendor/bin/Ubiquity serve -t=workerman -p=8080 -h=0.0.0.0
