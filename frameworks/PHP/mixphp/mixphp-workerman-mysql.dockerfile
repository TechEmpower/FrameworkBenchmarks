FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq && apt-get install -yqq git unzip wget curl build-essential php8.0-cli php8.0-mbstring php8.0-curl php8.0-xml php8.0-mysql > /dev/null

RUN apt-get install -yqq composer

RUN apt-get install -y php8.0-dev libevent-dev > /dev/null
RUN wget http://pear.php.net/go-pear.phar --quiet && php go-pear.phar
RUN pecl install event-3.0.4 > /dev/null && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini

COPY php-jit.ini /etc/php/8.0/cli/php.ini

ADD ./ /mixphp
WORKDIR /mixphp

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 2345

CMD php /mixphp/bin/workerman.php start
