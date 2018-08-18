FROM php:7.2

ENV SWOOLE_VERSION=4.0.4

RUN cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole

RUN docker-php-ext-install mysqli && docker-php-ext-enable mysqli

RUN apt-get update && apt-get install -y git unzip
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /hamlet
WORKDIR /hamlet

RUN chmod -R 777 /hamlet
RUN composer update

CMD php /hamlet/swoole.php
