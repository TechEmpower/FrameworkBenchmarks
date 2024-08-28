FROM php:8.3-cli

RUN apt-get update -yqq >> /dev/null
RUN apt-get install -y libevent-dev \
    libssl-dev \
    pkg-config \
    build-essential \
    unzip >> /dev/null

RUN docker-php-ext-install pdo_mysql \
    opcache \
    posix \
    pcntl \
    sockets >> /dev/null

RUN pecl install event >> /dev/null

RUN docker-php-ext-enable pdo_mysql opcache posix pcntl sockets
RUN docker-php-ext-enable --ini-name zz-event.ini event
RUN echo "opcache.enable_cli=1" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit=1205" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini
RUN echo "opcache.jit_buffer_size=128M" >> /usr/local/etc/php/conf.d/docker-php-ext-opcache.ini

# Initialize
WORKDIR /laravel
COPY --from=composer --link /usr/bin/composer /usr/local/bin/composer
COPY --link . .

RUN echo "PRP_HTTP_LISTEN=http://0.0.0.0:8080" >> .env
RUN echo "PRP_HTTP_COUNT=64" >> .env

RUN mkdir -p bootstrap/cache
RUN mkdir -p storage/logs
RUN mkdir -p storage/framework/sessions
RUN mkdir -p storage/framework/views
RUN mkdir -p storage/framework/cache

RUN composer install --quiet
RUN php artisan optimize

RUN composer require cclilshy/p-ripple-drive --quiet
RUN composer update --quiet

EXPOSE 8080

ENTRYPOINT ["php","artisan","p:server","start"]
