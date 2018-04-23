FROM php:7.2

ENV SWOOLE_VERSION=2.0.9

RUN cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure --quiet && make --quiet && make install --quiet \
        && docker-php-ext-enable swoole

ADD ./ /swoole
WORKDIR /swoole

CMD sed -i 's|NUMCORES|'"$(nproc)"'|g' swoole-server.php && \
    php swoole-server.php