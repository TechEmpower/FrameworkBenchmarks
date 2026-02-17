FROM php:8.3-cli

ARG DEBIAN_FRONTEND=noninteractive

ARG TFB_TEST_DATABASE
ENV TFB_TEST_DATABASE=${TFB_TEST_DATABASE}

RUN apt -yqq update > /dev/null && \
    apt upgrade -y > /dev/null && \
    apt -yqq install git unzip libevent-dev libssl-dev libpq-dev > /dev/null

RUN docker-php-ext-install -j$(nproc) opcache mysqli pcntl sockets pdo_pgsql > /dev/null

RUN pecl install event-3.1.4  > /dev/null && \
    echo "extension=event.so" > /usr/local/etc/php/conf.d/event.ini

WORKDIR /imi
COPY . .

COPY php.ini /usr/local/etc/php/

RUN chmod -R ug+rwx /imi/.runtime

COPY --from=composer/composer:2-bin --link /composer /usr/local/bin/composer
RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer require imiphp/imi-workerman:~2.1.0 -W --quiet
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-workerman.sh
