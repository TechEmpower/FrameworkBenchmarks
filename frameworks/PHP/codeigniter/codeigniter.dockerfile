FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get update > /dev/null && apt-get install -yqq nginx git unzip \
    php8.5-cli php8.5-fpm php8.5-mysql php8.5-mbstring php8.5-intl php8.5-curl > /dev/null

COPY --from=composer:latest /usr/bin/composer /usr/local/bin/composer

COPY ./deploy/conf/ /etc/php/8.5/fpm/
RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.5/fpm/pool.d/www.conf ; fi;

COPY ./deploy/nginx.conf /etc/nginx/

COPY --chown=www-data:www-data ./ /codeigniter/
WORKDIR /codeigniter
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --no-progress && \
    echo "opcache.preload=/codeigniter/preload.php" >> /etc/php/8.5/fpm/php.ini && \
    php spark optimize

EXPOSE 8080

CMD service php8.5-fpm start && \
    nginx
