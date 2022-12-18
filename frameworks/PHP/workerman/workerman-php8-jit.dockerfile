FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq php8.2-cli php8.2-pgsql php8.2-xml > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.2-dev libevent-dev git > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.2/cli/conf.d/event.ini
 
COPY php-jit.ini /etc/php/8.2/cli/php.ini

ADD ./ /workerman
WORKDIR /workerman

RUN sed -i "s|'/app.php|'/app-pg.php|g" server.php
RUN sed -i "s|init()|DbRaw::init()|g" server.php

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

CMD php /workerman/server.php start
