FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq && apt-get install -yqq php8.0-cli php8.0-mysql php8.0-xml

RUN apt-get install -yqq composer

RUN apt-get install -y php-pear php8.0-dev libevent-dev
RUN pecl install event-3.0.4 && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini
 
COPY php-jit.ini /etc/php/8.0/cli/php.ini

ADD ./ /mixphp
WORKDIR /mixphp

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 2345

CMD php /mixphp/bin/workerman.php start
