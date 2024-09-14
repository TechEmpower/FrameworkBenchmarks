FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq php8.3-cli php8.3-mysql  > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.3-dev libevent-dev git > /dev/null && \
    pecl install event-3.1.3 > /dev/null && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/event.ini

COPY --link php.ini /etc/php/8.3/cli/php.ini

WORKDIR /workerman
COPY --link . .

RUN composer require react/mysql "^0.6" --quiet
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

CMD php /workerman/server-async.php start
