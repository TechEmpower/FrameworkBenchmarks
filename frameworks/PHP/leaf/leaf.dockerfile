FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.3-cli php8.3-fpm php8.3-mysql php8.3-xml php8.3-curl php8.3-zip > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY --link deploy/conf/* /etc/php/8.3/fpm/

WORKDIR /leaf
COPY --link . .

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.3/fpm/php-fpm.conf ; fi;

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod -R 777 /leaf

EXPOSE 8080

CMD service php8.3-fpm start && \
    nginx -c /leaf/deploy/nginx.conf
