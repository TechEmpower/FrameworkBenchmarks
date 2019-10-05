FROM php:7.3

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

WORKDIR /swoole
COPY swoole-server-noasync.php swoole-server-noasync.php
COPY php.ini /usr/local/etc/php/

CMD sed -i 's|NUMCORES|'"$(nproc)"'|g' swoole-server-noasync.php && \
    php swoole-server-noasync.php
