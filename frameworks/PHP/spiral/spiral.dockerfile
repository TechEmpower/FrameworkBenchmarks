FROM php:8.4-cli

RUN apt-get update -yqq > /dev/null && apt-get install -yqq git unzip > /dev/null
COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

RUN docker-php-ext-install \
    opcache \
    pdo_mysql \
    sockets > /dev/null

# RoadRunner >= 2024.x.x requires protobuf extensions to be installed
ARG PROTOBUF_VERSION="4.30.1"
RUN pecl channel-update pecl.php.net
RUN MAKEFLAGS="-j $(nproc)" pecl install protobuf-${PROTOBUF_VERSION} > /dev/null

WORKDIR /spiral
COPY --link . .

# composer and opcache settings
COPY --link php/php.ini /usr/local/etc/php/
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

# pre-configure
RUN ./vendor/bin/rr get-binary > /dev/null 2>&1
RUN php app.php configure

EXPOSE 8080

CMD php app.php up > /dev/null 2>&1 && ./rr serve -o "http.pool.num_workers = 64"
