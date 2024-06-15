FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git php8.3-cli php8.3-pgsql php8.3-xml > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.3-dev libevent-dev > /dev/null
RUN pecl install event-3.1.3 > /dev/null && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/event.ini

COPY deploy/conf/cliphp.ini /etc/php/8.3/cli/php.ini

ADD ./ /kumbiaphp
WORKDIR /kumbiaphp

RUN git clone -b dev --single-branch --depth 1 https://github.com/KumbiaPHP/KumbiaPHP.git vendor/Kumbia

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

CMD php server.php start
