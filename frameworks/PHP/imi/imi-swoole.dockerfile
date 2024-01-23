FROM php:8.3-cli

ENV SWOOLE_VERSION 5.1.1
ARG TFB_TEST_DATABASE
ENV TFB_TEST_DATABASE=${TFB_TEST_DATABASE}

RUN apt -yqq update && \
    apt upgrade -y && \
    apt -yqq install git unzip libpq-dev

RUN docker-php-ext-install -j$(nproc) pdo_pgsql opcache mysqli

RUN cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure --enable-swoole-pgsql && make -j install \
        && docker-php-ext-enable swoole

COPY . /imi
COPY php.ini /usr/local/etc/php/

RUN chmod -R ug+rwx /imi/.runtime

WORKDIR /imi

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer
RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer require imiphp/imi-swoole:~2.1.0 -W
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-swoole.sh
