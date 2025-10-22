FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip curl \
    php8.4-cli php8.4-fpm php8.4-mysql  \
    php8.4-mbstring php8.4-xml php8.4-curl php8.4-dev > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

COPY --link deploy/conf/* /etc/php/8.4/fpm/
WORKDIR /symfony
COPY --link . .

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.4/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --no-scripts --quiet
RUN cp deploy/mysql/.env . && composer dump-env prod && bin/console cache:clear

RUN echo "opcache.preload=/symfony/var/cache/prod/App_KernelProdContainer.preload.php" >> /etc/php/8.4/fpm/php.ini

EXPOSE 8080

# Uncomment next line for Laravel console error logging to be viewable in docker logs
# RUN echo "catch_workers_output = yes" >> /etc/php/8.4/fpm/php-fpm.conf

RUN mkdir -p /run/php
CMD service php8.4-fpm start && \
    nginx -c /symfony/deploy/nginx.conf