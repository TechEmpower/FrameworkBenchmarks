  
FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php7.4 php7.4-common php7.4-cli php7.4-pgsql > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN apt-get install -y php-pear php-dev libevent-dev > /dev/null
RUN printf "\n\n /usr/lib/x86_64-linux-gnu/\n\n\nno\n\n\n" | pecl install event > /dev/null && echo "extension=event.so" > /etc/php/7.4/cli/conf.d/event.ini

COPY deploy/conf/php-async.ini /etc/php/7.4/cli/php.ini

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

RUN sed -i "s|'worker'|'pgsql'|g" /ubiquity/app/config/workerServices.php

RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php" >> /etc/php/7.4/cli/php.ini

CMD /ubiquity/vendor/bin/Ubiquity serve -t=workerman -p=8080 -h=0.0.0.0
