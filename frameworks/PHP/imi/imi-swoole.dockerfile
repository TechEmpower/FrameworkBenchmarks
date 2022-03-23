FROM php:8.1-cli

ENV SWOOLE_VERSION 4.8.3
ARG TFB_TEST_DATABASE
ENV TFB_TEST_DATABASE=${TFB_TEST_DATABASE}

RUN docker-php-ext-install -j$(nproc) opcache > /dev/null

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

RUN pecl update-channels

RUN pecl install swoole-${SWOOLE_VERSION} > /dev/null && \
    docker-php-ext-enable swoole

COPY . /imi
COPY php.ini /usr/local/etc/php/

RUN chmod -R ug+rwx /imi/.runtime

WORKDIR /imi

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer require imiphp/imi-swoole:~2.0 -W
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-swoole.sh
