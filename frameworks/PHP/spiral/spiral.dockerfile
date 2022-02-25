FROM php:7.4

RUN docker-php-ext-install pdo_mysql > /dev/null

ADD ./ /spiral
WORKDIR /spiral

# composer and opcache settings
COPY php/* /usr/local/etc/php/
RUN chmod +x /usr/local/etc/php/install-composer.sh && /usr/local/etc/php/install-composer.sh

# install dependencies
RUN apt-get update -yqq > /dev/null && apt-get install -yqq git unzip > /dev/null
RUN php composer.phar install --optimize-autoloader --classmap-authoritative --no-dev --quiet

# pre-configure
RUN ./vendor/bin/spiral get > /dev/null 2>&1
RUN php app.php configure > /dev/null 2>&1

EXPOSE 8080

CMD php app.php up > /dev/null 2>&1 && ./spiral serve -o "http.workers.pool.numWorkers = 64"
