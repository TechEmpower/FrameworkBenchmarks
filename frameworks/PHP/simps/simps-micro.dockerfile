FROM phpswoole/swoole:5.1.3-php8.3

RUN docker-php-ext-install pcntl opcache curl > /dev/null

WORKDIR /simps

COPY --link . .
COPY --link php.ini /usr/local/etc/php/

RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

EXPOSE 8080

ENTRYPOINT [ "php", "sbin/simps.php", "http:start" ]
