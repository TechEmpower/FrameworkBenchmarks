FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.4-fpm php8.4-mysql php8.4-xml php8.4-mbstring php8.4-intl php8.4-dev php8.4-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.4/fpm/
COPY deploy/conf/* /etc/php/8.4/cli/

ADD ./ /cakephp
WORKDIR /cakephp

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.4/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod -R 777 /cakephp 

CMD service php8.4-fpm start && \
    nginx -c /cakephp/deploy/nginx.conf

