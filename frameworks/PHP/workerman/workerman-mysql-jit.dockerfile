FROM ubuntu:24.04

ENV TEST_TYPE mysql
ENV PROCESS_MULTIPLIER 4
ENV EVENT_LOOP Event

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq php8.3-cli php8.3-mysql php8.3-xml > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.3-dev libevent-dev git > /dev/null && \
    pecl install event-3.1.4 > /dev/null && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/30-event.ini

WORKDIR /workerman
COPY --link . .

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
COPY php-jit.ini /etc/php/8.3/cli/conf.d/10-opcache.ini

EXPOSE 8080

CMD php /workerman/server.php start
