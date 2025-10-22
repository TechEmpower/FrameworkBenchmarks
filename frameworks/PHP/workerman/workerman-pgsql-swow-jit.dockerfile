FROM ubuntu:24.04

ENV TEST_TYPE pgsql-swow
ENV PROCESS_MULTIPLIER 2
ENV POOL_SIZE 16
ENV EVENT_LOOP Swow

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq php8.3-cli php8.3-pgsql php8.3-xml > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.3-dev git > /dev/null


WORKDIR /workerman
COPY --link . .


RUN composer require swow/swow > /dev/null
RUN ./vendor/bin/swow-builder --install > /dev/null
RUN echo extension=swow.so >  /etc/php/8.3/cli/conf.d/20-swow.ini
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
COPY php-jit.ini /etc/php/8.3/cli/conf.d/10-opcache.ini

EXPOSE 8080


CMD php /workerman/server.php start
