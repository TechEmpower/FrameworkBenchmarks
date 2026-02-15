FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq && apt-get install -yqq git curl php8.5-cli php8.5-mbstring php8.5-curl php8.5-xml php8.5-pgsql > /dev/null

RUN apt-get install -y php8.5-dev libevent-dev > /dev/null
RUN pecl install event-3.1.4 > /dev/null && echo "extension=event.so" > /etc/php/8.5/cli/conf.d/event.ini

COPY php-jit.ini /etc/php/8.5/cli/php.ini

WORKDIR /mixphp
COPY . .

RUN sed -i "s|Benchmark();|BenchmarkRaw();|g" /mixphp/routes/index.php

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer
RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer dumpautoload -o

RUN mkdir -p /mixphp/runtime/logs
RUN chmod -R 777 /mixphp/runtime/logs

EXPOSE 2345

CMD php /mixphp/bin/workerman.php start
