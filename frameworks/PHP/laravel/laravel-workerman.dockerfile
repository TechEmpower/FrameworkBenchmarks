FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq git unzip \
    php8.3-cli php8.3-mysql php8.3-mbstring php8.3-xml php8.3-curl > /dev/null

COPY --from=composer --link /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.3-dev libevent-dev > /dev/null
RUN pecl install event-3.1.3 > /dev/null && echo "extension=event.so" > /etc/php/8.3/cli/conf.d/event.ini

WORKDIR /laravel
COPY --link . .

RUN mkdir -p bootstrap/cache \
            storage/logs \
            storage/framework/sessions \
            storage/framework/views \
            storage/framework/cache

RUN composer require joanhey/adapterman --update-no-dev --no-scripts --quiet
RUN php artisan optimize

COPY --link deploy/conf/cli-php.ini /etc/php/8.3/cli/php.ini

EXPOSE 8080

ENTRYPOINT [ "php", "server-man.php", "start" ]
