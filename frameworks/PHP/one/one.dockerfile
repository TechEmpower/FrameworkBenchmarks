FROM php:8.0-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install opcache pdo_mysql > /dev/null

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

COPY . /one
COPY php.ini /usr/local/etc/php/

WORKDIR /one

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

RUN mkdir -p /one/App/RunCache
RUN chmod -R 777 /one/App/RunCache

CMD php App/swoole.php