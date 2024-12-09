FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php8.4-cli php8.4-pgsql php8.4-xml > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.4-dev libevent-dev git > /dev/null
RUN pecl install event-3.1.3 > /dev/null && echo "extension=event.so" > /etc/php/8.4/cli/conf.d/event.ini
 
COPY php.ini /etc/php/8.4/cli/php.ini

ADD ./ /mark
WORKDIR /mark

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN sed -i "s|opcache.jit=off|opcache.jit=tracing|g" /etc/php/8.4/cli/conf.d/10-opcache.ini

EXPOSE 8080

CMD php /mark/start.php start
