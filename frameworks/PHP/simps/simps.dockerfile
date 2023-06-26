FROM php:8.2-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install opcache pdo_mysql > /dev/null

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

WORKDIR /simps

COPY . /simps
COPY php.ini /usr/local/etc/php/

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

EXPOSE 8080

CMD php sbin/simps.php http:start
