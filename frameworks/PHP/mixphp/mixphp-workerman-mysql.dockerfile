FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq && apt-get install -yqq git unzip wget curl build-essential php8.1-cli php8.1-mbstring php8.1-curl php8.1-xml php8.1-mysql > /dev/null

RUN apt-get install -y php8.1-dev libevent-dev > /dev/null
RUN wget http://pear.php.net/go-pear.phar --quiet && php go-pear.phar
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.1/cli/conf.d/event.ini

COPY php-jit.ini /etc/php/8.1/cli/php.ini

ADD ./ /mixphp
WORKDIR /mixphp

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

RUN mkdir -p /mixphp/runtime/logs
RUN chmod -R 777 /mixphp/runtime/logs

EXPOSE 2345

CMD php /mixphp/bin/workerman.php start
