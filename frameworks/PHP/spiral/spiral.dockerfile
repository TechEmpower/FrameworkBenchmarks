FROM php:8.2.0-cli

RUN docker-php-ext-install pdo_mysql > /dev/null

# Workaround solution for installing ext-sockets for PHP 8.0
# See https://github.com/docker-library/php/issues/1245
RUN CFLAGS="$CFLAGS -D_GNU_SOURCE" docker-php-ext-install sockets > /dev/null

ADD ./ /spiral
WORKDIR /spiral

# composer and opcache settings
COPY php/* /usr/local/etc/php/
RUN chmod +x /usr/local/etc/php/install-composer.sh && /usr/local/etc/php/install-composer.sh

# install dependencies
RUN apt-get update -yqq > /dev/null && apt-get install -yqq git unzip > /dev/null
RUN php composer.phar install --optimize-autoloader --classmap-authoritative --no-dev

# pre-configure
RUN ./vendor/bin/rr get-binary > /dev/null 2>&1
RUN php app.php configure > /dev/null 2>&1

EXPOSE 8080

CMD php app.php up > /dev/null 2>&1 && ./rr serve -o "http.pool.num_workers = 64"
