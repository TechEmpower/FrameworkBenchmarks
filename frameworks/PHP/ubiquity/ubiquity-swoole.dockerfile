FROM php:7.3

ENV SWOOLE_VERSION=4.4.5

RUN cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql pcntl > /dev/null

COPY deploy/conf/php-async.ini /usr/local/etc/php/

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

CMD /ubiquity/vendor/bin/Ubiquity serve -t=swoole -p=8080 -h=0.0.0.0
