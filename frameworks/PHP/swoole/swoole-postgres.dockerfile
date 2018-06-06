FROM php:7.2

ENV SWOOLE_VERSION=2.1.3

RUN     apt-get update && apt-get install -y libpq-dev \
        && cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure --enable-coroutine --enable-coroutine-postgresql > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole
 
WORKDIR /swoole
COPY swoole-server.php swoole-server.php
COPY php.ini /usr/local/etc/php/

CMD sed -i 's|NUMCORES|'"$(nproc)"'|g' swoole-server.php && \
    php swoole-server.php
