FROM phpswoole/swoole:5.1.3-php8.3

RUN apt-get install -y libpq-dev \
    && docker-php-ext-configure pgsql -with-pgsql=/usr/local/pgsql \
    && docker-php-ext-install pdo_pgsql pgsql opcache pcntl > /dev/null

COPY deploy/conf/php-async.ini /usr/local/etc/php/php.ini

WORKDIR /ubiquity
ADD --link . .

RUN chmod -R 777 /ubiquity

RUN composer require phpmv/ubiquity-devtools:dev-master phpmv/ubiquity-swoole:dev-master --quiet

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R /ubiquity/.ubiquity/*

RUN echo "opcache.preload=/ubiquity/app/config/preloader.script.php" >> /usr/local/etc/php/php.ini
RUN echo "opcache.jit_buffer_size=128M\nopcache.jit=function\n" >> /usr/local/etc/php/php.ini

USER www-data

COPY deploy/conf/swoole/pgsql/swooleServices.php app/config/swooleServices.php

EXPOSE 8080

ENTRYPOINT [ "/ubiquity/vendor/bin/Ubiquity", "serve", "-t=swoole", "-p=8080", "-h=0.0.0.0" ]
