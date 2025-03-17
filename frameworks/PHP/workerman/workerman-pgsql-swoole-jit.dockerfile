FROM ubuntu:24.04

ENV TEST_TYPE pgsql-swoole
ENV SWOOLE_VERSION 5.1.5
ENV PROCESS_MULTIPLIER 2
ENV POOL_SIZE 16
ENV EVENT_LOOP Swoole

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq > /dev/null \
    && apt install -yqq software-properties-common > /dev/null \
    && LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null \
    && apt update -yqq > /dev/null \
    && apt install libbrotli-dev php8.3-cli php8.3-pdo-pgsql php8.3-dev libpq-dev php8.3-mbstring git -y > /dev/null \
    && cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
    && cd /tmp/swoole-src-${SWOOLE_VERSION} \
    && phpize > /dev/null \
    && ./configure --enable-swoole-pgsql > /dev/null \
    && make -j "$(nproc)" > /dev/null \
    && make install > /dev/null \
    && echo "extension=swoole.so" > /etc/php/8.3/cli/conf.d/50-swoole.ini \
    && echo "memory_limit=1024M" >> /etc/php/8.3/cli/php.ini

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

WORKDIR /workerman
COPY --link . .

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
COPY php-jit.ini /etc/php/8.3/cli/conf.d/10-opcache.ini

EXPOSE 8080

CMD php /workerman/server.php start
