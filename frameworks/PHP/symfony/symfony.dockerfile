FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip curl \
    php8.3-cli php8.3-fpm php8.3-pgsql  \
    php8.3-mbstring php8.3-xml php8.3-curl php8.3-dev > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.3/fpm/

ADD . /symfony
WORKDIR /symfony

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.3/fpm/php-fpm.conf ; fi;

RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log

ENV COMPOSER_ALLOW_SUPERUSER=1
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet --no-scripts
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

RUN echo "opcache.preload=/symfony/var/cache/prod/App_KernelProdContainer.preload.php" >> /etc/php/8.3/fpm/php.ini

EXPOSE 8080

# Uncomment next line for Laravel console error logging to be viewable in docker logs
# RUN echo "catch_workers_output = yes" >> /etc/php/8.3/fpm/php-fpm.conf

RUN mkdir -p /run/php
CMD service php8.3-fpm start && \
    nginx -c /symfony/deploy/nginx.conf