FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php8.0-cli php8.0-pgsql php8.0-xml php8.0-mbstring > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php-dev libevent-dev git > /dev/null
RUN pecl install event-3.0.5 > /dev/null && echo "extension=event.so" > /etc/php/8.0/cli/conf.d/event.ini

COPY php.ini /etc/php/8.0/cli/php.ini

ADD ./ /comet
WORKDIR /comet

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

CMD php /comet/app.php start
