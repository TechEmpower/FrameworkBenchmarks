FROM php:8.2-cli

RUN pecl install swoole > /dev/null && \
    docker-php-ext-enable swoole

RUN docker-php-ext-install pdo_mysql > /dev/null

ADD ./ /lumen
WORKDIR /lumen
COPY deploy/swoole/php.ini /usr/local/etc/php/

RUN mkdir -p /lumen/storage/framework/sessions
RUN mkdir -p /lumen/storage/framework/views
RUN mkdir -p /lumen/storage/framework/cache

RUN chmod -R 777 /lumen

# Install composer using the installation method documented at https://getcomposer.org/doc/faqs/how-to-install-composer-programmatically.md
# This method was chosen because composer is not part of the apt repositories that are in the default PHP 7.2 docker image
# Adding alternate apt php repos can potentially cause problems with extension compatibility between the php build from the docker image and the alternate php build
# An additional benefit of this method is that the correct version of composer will be used for the environment and version of the php system in the docker image
RUN deploy/swoole/install-composer.sh

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip > /dev/null

COPY deploy/swoole/composer* ./
RUN php composer.phar install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN echo "APP_SWOOLE=true" >> .env

RUN chmod -R 777 /lumen

EXPOSE 8080

CMD php artisan swoole:http start
