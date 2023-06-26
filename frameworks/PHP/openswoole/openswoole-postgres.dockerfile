FROM php:8.2-cli

RUN apt-get update && apt-get install -y git > /dev/null

RUN docker-php-ext-install opcache  > /dev/null

ENV VERSION 22.0.0

RUN     apt-get update && apt-get install -y libpq-dev \
        && cd /tmp && curl -sSL "https://github.com/openswoole/ext-openswoole/archive/v${VERSION}.tar.gz" | tar xzf - \
        && cd ext-openswoole-${VERSION} \
        && phpize && ./configure --with-postgres > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable openswoole
 
COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY php.ini /usr/local/etc/php/

ADD ./ /openswoole
WORKDIR /openswoole

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

CMD php /openswoole/openswoole-server-postgres.php
