FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip curl \
    php8.2-cli php8.2-fpm php8.2-mysql  \
    php8.2-mbstring php8.2-xml php8.2-curl php8.2-dev > /dev/null

RUN curl -sSL https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

COPY deploy/conf/* /etc/php/8.2/fpm/
RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.2/fpm/php-fpm.conf ; fi;

WORKDIR /symfony
ADD ./composer.json /symfony/
RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log
RUN composer install --no-dev --no-scripts

# downgrade to doctrine-dbal 2.12 => due to a bug in version 2.13
# see https://github.com/doctrine/dbal/issues/4603
#RUN composer require doctrine/orm:2.10.2 -W
#RUN composer require doctrine/dbal:2.12.x -W

ADD . /symfony
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --no-dev --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env prod

# removes hardcoded option `ATTR_STATEMENT_CLASS` conflicting with `ATTR_PERSISTENT`. Hack not needed when upgrading to Doctrine 3
# see https://github.com/doctrine/dbal/issues/2315
#RUN sed -i '/PDO::ATTR_STATEMENT_CLASS/d' ./vendor/doctrine/dbal/lib/Doctrine/DBAL/Driver/PDOConnection.php

RUN php bin/console cache:clear
RUN echo "opcache.preload=/symfony/var/cache/prod/App_KernelProdContainer.preload.php" >> /etc/php/8.2/fpm/php.ini

CMD service php8.2-fpm start && \
    nginx -c /symfony/deploy/nginx.conf
