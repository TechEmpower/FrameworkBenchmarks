FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq unzip \
    php8.3-cli php8.3-pgsql php8.3-mbstring php8.3-xml php8.3-curl > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.3-dev libevent-dev > /dev/null && \
    pecl install event-3.1.3 > /dev/null && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/event.ini

WORKDIR /symfony
COPY --link . .

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --no-scripts --quiet
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

COPY --link deploy/conf/cli-php.ini /etc/php/8.3/cli/php.ini

EXPOSE 8080

ENTRYPOINT [ "php", "server.php", "start" ]
