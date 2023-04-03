FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null

RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq wget git libxml2-dev systemtap-sdt-dev \
                    zlib1g-dev libpcre3-dev libargon2-0-dev libsodium-dev \
                    php8.1-cli php8.1-dev libphp8.1-embed php8.1-pgsql > /dev/null
ADD . .

ENV NGINX_VERSION 1.23.4

RUN git clone -b v0.0.26 --single-branch --depth 1 https://github.com/rryqszq4/ngx-php.git > /dev/null

RUN wget -q http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    export PHP_LIB=/usr/lib && \
    ./configure --user=www --group=www \
            --prefix=/nginx \
            --with-ld-opt="-Wl,-rpath,$PHP_LIB" \
            --add-module=/ngx-php/third_party/ngx_devel_kit \
            --add-module=/ngx-php > /dev/null && \
    make > /dev/null && make install > /dev/null

RUN sed -i "s|app.php|app-pg.php|g" /deploy/nginx.conf

RUN export WORKERS=$(( 4 * $(nproc) )) && \
    sed -i "s|worker_processes  auto|worker_processes $WORKERS|g" /deploy/nginx.conf

EXPOSE 8080

CMD /nginx/sbin/nginx -c /deploy/nginx.conf
