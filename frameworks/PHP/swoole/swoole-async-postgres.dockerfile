FROM ubuntu:24.04

ENV ENABLE_COROUTINE 1
ENV CPU_MULTIPLES 1
ENV DATABASE_DRIVER pgsql

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq > /dev/null \
    && apt install -yqq software-properties-common > /dev/null \
    && LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null \
    && apt update -yqq > /dev/null \
    && apt install git libbrotli-dev php8.4-cli php8.4-pdo-pgsql php8.4-dev libpq-dev -y > /dev/null \
    && cd /tmp && git clone https://github.com/swoole/swoole-src.git \
    && cd /tmp/swoole-src/ && git checkout feat_llhttp \
    && phpize > /dev/null \
    && ./configure --enable-swoole-pgsql > /dev/null \
    && make -j "$(nproc)" > /dev/null \
    && make install > /dev/null \
    && echo "extension=swoole.so" > /etc/php/8.4/cli/conf.d/50-swoole.ini \
    && echo "memory_limit=1024M" >> /etc/php/8.4/cli/php.ini

WORKDIR /swoole

ADD ./swoole-server.php /swoole
ADD ./database.php /swoole

COPY 10-opcache.ini /etc/php/8.4/cli/conf.d/10-opcache.ini

EXPOSE 8080
CMD php /swoole/swoole-server.php
