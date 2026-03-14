FROM phpswoole/swoole:5.1-php8.3

ARG TFB_TEST_DATABASE
ENV TFB_TEST_DATABASE=${TFB_TEST_DATABASE}

RUN docker-php-ext-install -j$(nproc) opcache mysqli > /dev/null

WORKDIR /imi
COPY . .

COPY php.ini /usr/local/etc/php/

RUN chmod -R ug+rwx /imi/.runtime

RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer require imiphp/imi-swoole:~2.1.0 -W --quiet
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-swoole.sh
