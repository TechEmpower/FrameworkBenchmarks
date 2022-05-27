FROM php:8.1-cli

ARG TFB_TEST_DATABASE
ENV TFB_TEST_DATABASE=${TFB_TEST_DATABASE}

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip libevent-dev libssl-dev libpq-dev > /dev/null

RUN docker-php-ext-install -j$(nproc) opcache mysqli pcntl sockets pdo_pgsql > /dev/null

RUN pecl update-channels

RUN pecl install event > /dev/null && \
    echo "extension=event.so" > /usr/local/etc/php/conf.d/event.ini

COPY . /imi
COPY php.ini /usr/local/etc/php/

RUN chmod -R ug+rwx /imi/.runtime

WORKDIR /imi

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer require imiphp/imi-workerman:~2.0 -W
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-workerman.sh
