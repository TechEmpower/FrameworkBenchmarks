FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq git unzip \
    php8.2-cli php8.2-mysql php8.2-mbstring php8.2-xml php8.2-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

RUN apt-get install -y php-pear php8.2-dev libevent-dev > /dev/null
RUN pecl install event-3.0.8 > /dev/null && echo "extension=event.so" > /etc/php/8.2/cli/conf.d/event.ini

EXPOSE 8080

WORKDIR /symfony
ADD ./composer.json /symfony/
RUN mkdir -m 777 -p /symfony/var/cache/{dev,prod} /symfony/var/log
RUN composer install --no-dev --no-scripts --quiet

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

COPY deploy/conf/cli-php.ini /etc/php/8.2/cli/php.ini

CMD php server.php start
