FROM phpswoole/swoole:5.1.3-php8.3

RUN docker-php-ext-install pcntl opcache > /dev/null

# RUN pecl install --force redis

COPY deploy/php-async.ini /usr/local/etc/php/php.ini

COPY . /fomo

WORKDIR /fomo

RUN composer install --no-dev --quiet
RUN composer dump-autoload --optimize --quiet

RUN chmod -R 777 /fomo/storage

# USER www-data

EXPOSE 9000

ENTRYPOINT [ "php", "engineer", "server:start" ]
