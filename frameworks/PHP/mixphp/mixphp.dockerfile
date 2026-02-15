FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq && apt-get install -yqq git curl nginx php8.5-fpm php8.5-mysql > /dev/null

COPY deploy/conf/* /etc/php/8.5/fpm/

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.5/fpm/php-fpm.conf ; fi;

WORKDIR /mixphp
COPY . .

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer
RUN composer install --no-dev --classmap-authoritative --quiet
RUN composer dumpautoload -o

RUN mkdir -p /mixphp/runtime/logs
RUN chmod -R 777 /mixphp/runtime/logs

EXPOSE 8080

CMD service php8.5-fpm start && \
    nginx -c /mixphp/deploy/nginx.conf
