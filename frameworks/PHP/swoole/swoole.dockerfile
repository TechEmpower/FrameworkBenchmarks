FROM php:7.3

ENV SWOOLE_VERSION=4.3.0

RUN cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

WORKDIR /swoole
COPY swoole-server.php swoole-server.php
COPY php.ini /usr/local/etc/php/

CMD sed -i 's|NUMCORES|'"$(nproc)"'|g' swoole-server.php && \
    php swoole-server.php
