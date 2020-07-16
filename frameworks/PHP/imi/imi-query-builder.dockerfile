FROM php:7.4

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

RUN echo "zend_extension=opcache.so" >> /usr/local/etc/php/php.ini

COPY . /imi
COPY php.ini /usr/local/etc/php/

WORKDIR /imi

RUN chmod -R ug+rwx /imi/.runtime

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

CMD php vendor/bin/imi server/start
