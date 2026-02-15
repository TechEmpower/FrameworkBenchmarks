FROM phpswoole/swoole:php8.5

RUN docker-php-ext-install pcntl curl > /dev/null

WORKDIR /simps

COPY --link . .
COPY --link php.ini /usr/local/etc/php/

RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer dumpautoload -o

EXPOSE 8080

ENTRYPOINT [ "php", "sbin/simps.php", "http:start" ]
