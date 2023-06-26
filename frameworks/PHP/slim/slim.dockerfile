FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.2 php8.2-common php8.2-cli php8.2-fpm php8.2-mysql php8.2-xml php8.2-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY deploy/conf/* /etc/php/8.2/fpm/

ADD ./ /slim
WORKDIR /slim

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.2/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod -R 777 /slim

EXPOSE 8080

CMD service php8.2-fpm start && \
    nginx -c /slim/deploy/nginx.conf
