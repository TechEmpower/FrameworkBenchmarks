FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq wget git unzip libxml2-dev cmake make systemtap-sdt-dev \
                    zlibc zlib1g zlib1g-dev libpcre3 libpcre3-dev libargon2-0-dev libsodium-dev \
                    php7.4 php7.4-common php7.4-dev libphp7.4-embed php7.4-pgsql nginx > /dev/null

ADD ./ ./

ENV NGINX_VERSION=1.19.2

RUN git clone -b v0.0.24 --single-branch --depth 1 https://github.com/rryqszq4/ngx_php7.git > /dev/null

RUN wget -q http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    export PHP_LIB=/usr/lib && \ 
    ./configure --user=www-data --group=www-data \
            --prefix=/nginx \
            --with-ld-opt="-Wl,-rpath,$PHP_LIB" \
            --add-module=/ngx_php7/third_party/ngx_devel_kit \
            --add-module=/ngx_php7 > /dev/null && \
    make > /dev/null && make install > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN composer config -g repo.packagist composer https://packagist.phpcomposer.com

RUN composer require phpmv/ubiquity-ngx:dev-master --quiet

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN chmod 777 -R app/cache/*

COPY /deploy/conf/ngx/pgsql/raw/ngxServices.php /app/config/ngxServices.php

RUN echo "opcache.preload=/app/config/preloader.script.php" >> /deploy/conf/php.ini

RUN export WORKERS=$(( 4 * $(nproc) )) && \
    sed -i "s|worker_processes  auto|worker_processes $WORKERS|g" /deploy/conf/ngx/nginx.conf

CMD /nginx/sbin/nginx -c /deploy/conf/ngx/nginx.conf
