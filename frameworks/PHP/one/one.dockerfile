FROM php:8.0-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install opcache pdo_mysql bcmath > /dev/null

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

COPY . /one
COPY php.ini /usr/local/etc/php/
RUN echo "opcache.enable=1" >> /usr/local/etc/php/php.ini
RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/php.ini
RUN echo "pcre.jit=1" >> /usr/local/etc/php/php.ini
RUN echo "opcache.jit=1205" >> /usr/local/etc/php/php.ini
RUN echo "opcache.jit_buffer_size=256M" >> /usr/local/etc/php/php.ini

RUN php -v && php -i | grep opcache

WORKDIR /one

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

RUN mkdir -p /one/App/RunCache
RUN chmod -R 777 /one/App/RunCache

EXPOSE 8080

CMD php App/swoole.php
