FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip curl \
    php8.0-cli php8.0-fpm php8.0-mysql  \
    php8.0-mbstring php8.0-xml php8.0-curl > /dev/null

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

COPY deploy/conf/* /etc/php/8.0/fpm/
RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.0/fpm/php-fpm.conf ; fi;

WORKDIR /symfony
ADD ./composer.json /symfony/
RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log
RUN COMPOSER_ALLOW_SUPERUSER=1 composer install --no-dev --no-scripts --quiet
ADD . /symfony
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --no-dev --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env prod

# removes hardcoded option `ATTR_STATEMENT_CLASS` conflicting with `ATTR_PERSISTENT`. Hack not needed when upgrading to Doctrine 3
# see https://github.com/doctrine/dbal/issues/2315
RUN sed -i '/PDO::ATTR_STATEMENT_CLASS/d' ./vendor/doctrine/dbal/lib/Doctrine/DBAL/Driver/PDOConnection.php

RUN php bin/console cache:clear
RUN echo "opcache.preload=/symfony/var/cache/prod/App_KernelProdContainer.preload.php" >> /etc/php/8.0/fpm/php.ini

EXPOSE 8080

CMD service php8.0-fpm start && \
    nginx -c /symfony/deploy/nginx.conf
