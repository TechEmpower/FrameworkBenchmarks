FROM phpswoole/swoole:6.0.2-php8.4

RUN docker-php-ext-install pcntl opcache bcmath > /dev/null

WORKDIR /mixphp
COPY --link . .
COPY --link php.ini /usr/local/etc/php/
RUN echo "opcache.enable=1" >> /usr/local/etc/php/php.ini
RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/php.ini
RUN echo "pcre.jit=1" >> /usr/local/etc/php/php.ini
RUN echo "opcache.jit=1205" >> /usr/local/etc/php/php.ini
RUN echo "opcache.jit_buffer_size=256M" >> /usr/local/etc/php/php.ini

RUN php -v && php -i | grep opcache

RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

RUN mkdir -p /mixphp/runtime/logs
RUN chmod -R 777 /mixphp/runtime/logs

EXPOSE 9501

ENTRYPOINT [ "php", "/mixphp/bin/swoole.php", "start" ]
