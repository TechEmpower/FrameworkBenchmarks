FROM php:7.4

RUN apt-get update > /dev/null

RUN pecl install swoole-4.4.7 > /dev/null && \
    docker-php-ext-enable swoole

RUN apt-get install -y libpq-dev \
    && docker-php-ext-configure pgsql -with-pgsql=/usr/local/pgsql \
    && docker-php-ext-install pdo pdo_pgsql pgsql > /dev/null

COPY deploy/conf/php-async.ini /usr/local/etc/php/

ADD ./ /ubiquity
WORKDIR /ubiquity

RUN chmod -R 777 /ubiquity

RUN ["chmod", "+x", "deploy/run/install-composer.sh"]

RUN deploy/run/install-composer.sh

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

RUN php composer.phar require phpmv/ubiquity-devtools:dev-techempower-benchmarks phpmv/ubiquity-swoole:dev-techempower-benchmarks --quiet

RUN php composer.phar install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/.ubiquity/*

CMD /ubiquity/vendor/bin/Ubiquity serve -t=swoole -p=8080 -h=0.0.0.0
