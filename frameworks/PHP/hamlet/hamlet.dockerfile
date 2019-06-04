FROM ubuntu:19.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -y \
    && apt install -y gnupg ca-certificates apt-transport-https wget curl \
    && wget -q https://packages.sury.org/php/apt.gpg -O- | apt-key add - \
    && echo "deb https://packages.sury.org/php/ stretch main" | tee /etc/apt/sources.list.d/php.list \
    && apt-get update -y \
    && apt-get install -y nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql

COPY deploy/fpm/php-fpm.conf /etc/php/7.3/fpm/php-fpm.conf
COPY deploy/fpm/php.ini /etc/php/7.3/fpm/php.ini

ADD ./ /app
WORKDIR /app

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.3/fpm/php-fpm.conf ; fi;

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
    && composer update --no-dev

CMD service php7.3-fpm start \
    && nginx -c /app/deploy/fpm/nginx.conf -g "daemon off;"
