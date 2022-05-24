FROM php:8.1-cli

RUN docker-php-ext-install opcache pcntl pdo_mysql
COPY deploy/conf/cli-php.ini /usr/local/etc/php/php.ini

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git

ADD ./ /yii2
WORKDIR /yii2

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/bin --filename=composer 

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev

RUN sed -i 's|(new  yii\\web\\Application|//(new  yii\\web\\Application|' app/index.php
RUN sed -i 's|(headers_sent($file, $line))|(headers_sent())|g' vendor/yiisoft/yii2/web/Response.php

RUN chmod -R 777 /yii2

CMD php server.php start
