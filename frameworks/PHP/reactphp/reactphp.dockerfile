FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip wget curl build-essential \
    php8.3-cli php8.3-mbstring php8.3-dev php8.3-xml > /dev/null

# An extension is required!
# We deal with concurrencies over 1k, which stream_select doesn't support.
RUN apt-get install -yqq libuv1-dev > /dev/null \
    && pecl install uv-beta > /dev/null \
    && echo "extension=uv.so" > /etc/php/8.3/cli/conf.d/uv.ini

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.3/cli/conf.d/

WORKDIR /reactphp

COPY composer.json .
RUN composer install --prefer-dist --optimize-autoloader --no-dev --quiet

COPY . .

EXPOSE 8080

ENTRYPOINT ["/usr/bin/php"]
CMD ["server.php"]
