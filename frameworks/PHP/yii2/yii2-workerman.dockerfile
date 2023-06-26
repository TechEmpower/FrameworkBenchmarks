FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git php8.2-cli php8.2-mysql php8.2-mbstring php8.2-xml > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.2-dev libevent-dev > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.2/cli/conf.d/event.ini

COPY deploy/conf/cli-php.ini /etc/php/8.2/cli/php.ini

ADD ./ /yii2
WORKDIR /yii2

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN sed -i 's|(new  yii\\web\\Application|//(new  yii\\web\\Application|' app/index.php
RUN sed -i 's|(headers_sent($file, $line))|(headers_sent())|g' vendor/yiisoft/yii2/web/Response.php

RUN chmod -R 777 /yii2

CMD php server.php start
