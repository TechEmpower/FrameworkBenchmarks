FROM openswoole/swoole:25.2-php8.4

RUN docker-php-ext-install opcache pdo_pgsql > /dev/null

ENV TINI_SUBREAPER 1
#ENV DISABLE_DEFAULT_SERVER 1

COPY php.ini /usr/local/etc/php/

WORKDIR /var/www
COPY --link . .

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

EXPOSE 8080

RUN php --ri openswoole

CMD ["php", "/var/www/openswoole-server-postgres.php"]
