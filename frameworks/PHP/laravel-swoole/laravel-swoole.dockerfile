FROM php:7.2

ENV SWOOLE_VERSION=4.1.2

RUN cd /tmp && curl -sSL "https://github.com/swoole/swoole-src/archive/v${SWOOLE_VERSION}.tar.gz" | tar xzf - \
        && cd swoole-src-${SWOOLE_VERSION} \
        && phpize && ./configure > /dev/null && make > /dev/null && make install > /dev/null \
        && docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

ADD ./ /laravel-swoole
WORKDIR /laravel-swoole
COPY php.ini /usr/local/etc/php/

RUN mkdir -p /laravel-swoole/bootstrap/cache
RUN mkdir -p /laravel-swoole/storage/framework/sessions
RUN mkdir -p /laravel-swoole/storage/framework/views
RUN mkdir -p /laravel-swoole/storage/framework/cache
RUN mkdir -p /laravel-swoole/storage/logs

RUN chmod -R 777 /laravel-swoole

# Install composer using the installation method documented at https://getcomposer.org/doc/faqs/how-to-install-composer-programmatically.md
# This method was chosen because composer is not part of the apt repositories that are in the default PHP 7.2 docker image
# Adding alternate apt php repos can potentially cause problems with extension compatibility between the php build from the docker image and the alternate php build
# An additional benefit of this method is that the correct version of composer will be used for the environment and version of the php system in the docker image
RUN ./install-composer.sh

RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq git unzip > /dev/null
RUN php composer.phar install --quiet

RUN php artisan config:cache
RUN php artisan route:cache

RUN chmod -R 777 /laravel-swoole

CMD php artisan swoole:http start
