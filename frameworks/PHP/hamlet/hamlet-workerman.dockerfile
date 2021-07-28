FROM ubuntu:20.10

ENV PHP_VERSION 8.0
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php${PHP_VERSION} php${PHP_VERSION}-common php${PHP_VERSION}-cli php${PHP_VERSION}-pdo-mysql  > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN apt-get install -y php-pear php${PHP_VERSION}-dev libevent-dev > /dev/null
RUN pecl install event-3.0.4 > /dev/null && echo "extension=event.so" > /etc/php/${PHP_VERSION}/cli/conf.d/event.ini

COPY deploy/fpm/php.ini /etc/php/${PHP_VERSION}/fpm/php.ini

ADD ./ /hamlet
WORKDIR /hamlet
COPY ./composer-workerman.json composer.json

RUN composer update --no-dev --quiet

EXPOSE 8080

CMD php /hamlet/workerman.php start
