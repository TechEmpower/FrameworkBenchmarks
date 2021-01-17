FROM php:8.0-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install bcmath pdo_mysql opcache > /dev/null

RUN pecl install redis > /dev/null && \
    docker-php-ext-enable redis

RUN apt -yqq update > /dev/null && \
    apt -yqq install git unzip > /dev/null

RUN apt -yqq install redis-server > /dev/null

RUN echo "opcache.enable_cli=On" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit=Off" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

COPY . /imi
COPY php.ini /usr/local/etc/php/

WORKDIR /imi
COPY .env-with-redis .env

RUN chmod -R ug+rwx /imi/.runtime

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
RUN composer install --no-dev --classmap-authoritative --quiet > /dev/null
RUN composer dumpautoload -o

EXPOSE 8080

CMD ./run-with-redis.sh
