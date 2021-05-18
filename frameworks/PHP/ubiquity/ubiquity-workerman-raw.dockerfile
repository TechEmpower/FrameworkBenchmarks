  
FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git php8.0-cli php8.0-pgsql php8.0-xml > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN apt-get install -y php-pear php8.0-dev libevent-dev > /dev/null
RUN pecl install event-3.0.2 > /dev/null && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini

COPY deploy/conf/php-async.ini /etc/php/8.0/cli/php.ini

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN chmod -R 777 /ubiquity

RUN ["chmod", "+x", "deploy/run/install-composer.sh"]

RUN deploy/run/install-composer.sh

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

RUN php composer.phar require phpmv/ubiquity-devtools:dev-master phpmv/ubiquity-workerman:dev-master --quiet

RUN php composer.phar install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/.ubiquity/*

COPY deploy/conf/workerman/pgsql/raw/workerServices.php app/config/workerServices.php

RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php\n" >> /etc/php/8.0/cli/php.ini
RUN echo "opcache.jit_buffer_size=128M\nopcache.jit=tracing\n" >> /etc/php/8.0/cli/php.ini

EXPOSE 8080

CMD /ubiquity/vendor/bin/Ubiquity serve -t=workerman -p=8080 -h=0.0.0.0
