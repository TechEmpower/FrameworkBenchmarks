FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip wget curl build-essential \
    php8.4-cli php8.4-mbstring php8.4-dev php8.4-xml > /dev/null

# An extension is required!
# We deal with concurrencies over 1k, which stream_select doesn't support.
# libuv
# RUN apt-get install -yqq libuv1-dev > /dev/null \
#     && pecl install uv-beta > /dev/null \
#     && echo "extension=uv.so" > /etc/php/8.4/cli/conf.d/uv.ini

# libevent
RUN apt-get install -y libevent-dev > /dev/null \
    && pecl install event-3.1.4 > /dev/null \
    && echo "extension=event.so" > /etc/php/8.4/cli/conf.d/event.ini

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY --link deploy/conf/* /etc/php/8.4/cli/conf.d/

WORKDIR /reactphp
COPY --link . .

RUN composer install --prefer-dist --optimize-autoloader --no-dev

EXPOSE 8080

ENTRYPOINT ["/usr/bin/php"]
CMD ["server.php"]
