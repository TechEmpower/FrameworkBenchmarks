FROM php:8.0-zts

ENV PHP_VERSION 8.0
ENV PARALLEL_VERSION 360c667b7632a639a983f17c5d97b92cbe4f7c95

RUN docker-php-ext-install pdo_mysql > /dev/null && docker-php-ext-enable pdo_mysql
RUN docker-php-ext-install sockets   > /dev/null && docker-php-ext-enable sockets
RUN docker-php-ext-install pcntl     > /dev/null && docker-php-ext-enable pcntl

RUN apt-get update -yqq > /dev/null \
    && apt-get install -yqq git unzip libevent-dev libssl-dev > /dev/null

RUN git clone https://github.com/krakjoe/parallel \
    && cd parallel \
    && git checkout 360c667b7632a639a983f17c5d97b92cbe4f7c95 \
    && phpize > /dev/null \
    && ./configure --enable-parallel > /dev/null \
    && make > /dev/null \
    && make install > /dev/null

RUN pecl install event-3.0.5 > /dev/null \
    && echo "extension=event.so" > /usr/local/etc/php/conf.d/event.ini

COPY deploy/fpm/php.ini /usr/local/etc/php/conf.d/hamlet.ini

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /hamlet
WORKDIR /hamlet
COPY ./composer-workerman.json composer.json

RUN composer update --no-dev --quiet

EXPOSE 8080

CMD php /hamlet/workerman.php start
