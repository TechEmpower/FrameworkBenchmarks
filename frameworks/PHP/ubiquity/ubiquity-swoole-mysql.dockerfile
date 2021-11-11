FROM php:8.0

RUN apt-get update > /dev/null

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql opcache pcntl > /dev/null

COPY deploy/conf/php-async.ini /usr/local/etc/php/php.ini

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN chmod -R 777 /ubiquity

RUN ["chmod", "+x", "deploy/run/install-composer.sh"]

RUN deploy/run/install-composer.sh

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

RUN php composer.phar require phpmv/ubiquity-devtools:dev-master phpmv/ubiquity-swoole:dev-master --quiet

RUN php composer.phar install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/.ubiquity/*

RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php" >> /usr/local/etc/php/php.ini
RUN echo "opcache.jit_buffer_size=128M\nopcache.jit=tracing\n" >> /usr/local/etc/php/php.ini

USER www-data

COPY deploy/conf/swoole/mysql/swooleServices.php app/config/swooleServices.php

EXPOSE 8080

CMD /ubiquity/vendor/bin/Ubiquity serve -t=swoole -p=8080 -h=0.0.0.0
