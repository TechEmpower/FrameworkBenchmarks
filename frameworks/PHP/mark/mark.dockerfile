FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php8.1-cli php8.1-pgsql php8.1-xml > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.1-dev libevent-dev git > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.1/cli/conf.d/event.ini
 
COPY php.ini /etc/php/8.1/cli/php.ini

ADD ./ /mark
WORKDIR /mark

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

CMD php /mark/start.php start
