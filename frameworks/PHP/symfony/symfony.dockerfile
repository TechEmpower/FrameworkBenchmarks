FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php7.4 php7.4-common php7.4-cli php7.4-fpm php7.4-mysql  > /dev/null
RUN apt-get install -yqq php7.4-mbstring php7.4-xml  > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/7.4/fpm/
RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.4/fpm/php-fpm.conf ; fi;

WORKDIR /symfony
ADD ./composer.json ./composer.lock /symfony/
RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log
RUN COMPOSER_ALLOW_SUPERUSER=1 composer install --no-dev --no-scripts
ADD . /symfony
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-autoload --no-dev --classmap-authoritative
RUN COMPOSER_ALLOW_SUPERUSER=1 composer dump-env prod

# removes hardcoded option `ATTR_STATEMENT_CLASS` conflicting with `ATTR_PERSISTENT`. Hack not needed when upgrading to Doctrine 3
# see https://github.com/doctrine/dbal/issues/2315
RUN sed -i '/PDO::ATTR_STATEMENT_CLASS/d' ./vendor/doctrine/dbal/lib/Doctrine/DBAL/Driver/PDOConnection.php

RUN php bin/console cache:clear
RUN echo "opcache.preload=/symfony/var/cache/prod/App_KernelProdContainer.preload.php" >> /etc/php/7.4/fpm/php.ini

CMD service php7.4-fpm start && \
    nginx -c /symfony/deploy/nginx.conf -g "daemon off;"
