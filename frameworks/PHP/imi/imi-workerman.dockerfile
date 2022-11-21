FROM php:8.1-cli

ARG TFB_TEST_DATABASE
ENV TFB_TEST_DATABASE=${TFB_TEST_DATABASE}

RUN apt -yqq update && \
    apt upgrade -y && \
    apt -yqq install git unzip libevent-dev libssl-dev libpq-dev

RUN docker-php-ext-install -j$(nproc) opcache mysqli pcntl sockets pdo_pgsql

RUN pecl update-channels

RUN pecl install event && \
    echo "extension=event.so" > /usr/local/etc/php/conf.d/event.ini

COPY . /imi
COPY php.ini /usr/local/etc/php/

RUN chmod -R ug+rwx /imi/.runtime

WORKDIR /imi

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer require imiphp/imi-workerman:~2.1.0 -W
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-workerman.sh
