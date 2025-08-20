FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get update -yqq > /dev/null && apt-get install -yqq git \
    php8.4-cli php8.4-mysql php8.4-mbstring php8.4-xml php8.4-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.4-dev libevent-dev > /dev/null
RUN pecl install event-3.1.4 > /dev/null && echo "extension=event.so" > /etc/php/8.4/cli/conf.d/event.ini

COPY deploy/conf/adapterman-php.ini /etc/php/8.4/cli/conf.d/20-adapterman.ini

WORKDIR /wolff/bench
COPY --link . .

EXPOSE 8080

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet 
RUN composer require joanhey/adapterman:^0.7 --quiet

RUN chmod -R 777 /wolff

CMD php server.php start

