FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php > /dev/null && \
    apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.5-bcmath php8.5-cli php8.5-fpm php8.5-mysql php8.5-mbstring php8.5-xml php8.5-curl php8.5-intl > /dev/null

# Use Jemalloc for optimize
RUN apt install libjemalloc2
ENV LD_PRELOAD=libjemalloc.so.2

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

COPY --link deploy/conf/* /etc/php/8.5/fpm/
WORKDIR /laravel
COPY --link . .

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.5/fpm/php-fpm.conf ; fi;

RUN mkdir -p bootstrap/cache \
            storage/logs \
            storage/framework/sessions \
            storage/framework/views \
            storage/framework/cache

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet
RUN php artisan optimize

EXPOSE 8080

# Uncomment next line for Laravel console error logging to be viewable in docker logs
# RUN echo "catch_workers_output = yes" >> /etc/php/8.5/fpm/php-fpm.conf

CMD service php8.5-fpm start && \
    nginx -c /laravel/deploy/nginx.conf
