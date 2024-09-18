FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get update -yqq > /dev/null && apt-get install -yqq git \
    php8.3-cli php8.3-mysql php8.3-mbstring php8.3-xml php8.3-curl php8.3-zip > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y libevent-dev php8.3-dev > /dev/null \
    && pecl install event-3.1.4 > /dev/null \
    && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/event.ini

COPY --link deploy/conf/cli-php.ini /etc/php/8.3/cli/php.ini

WORKDIR /leaf
COPY --link . .

EXPOSE 8080

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN composer require joanhey/adapterman:^0.6 --quiet


RUN sed -i 's|app()->run(); //| //$app->run(); //|g' index.php

RUN chmod -R 777 /leaf

CMD php server.php start
