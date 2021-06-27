FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq wget git unzip libxml2-dev cmake make systemtap-sdt-dev \
                    zlib1g-dev libpcre3-dev libargon2-0-dev libsodium-dev \
                    php8.0-cli php8.0-dev libphp8.0-embed php8.0-mysql nginx > /dev/null
ADD ./ ./

ENV NGINX_VERSION 1.21.0

RUN git clone -b v0.0.25 --single-branch --depth 1 https://github.com/rryqszq4/ngx_php7.git > /dev/null

RUN wget -q http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    export PHP_LIB=/usr/lib && \ 
    bash ./configure --user=www --group=www \
            --prefix=/nginx \
            --with-ld-opt="-Wl,-rpath,$PHP_LIB" \
            --add-module=/ngx_php7/third_party/ngx_devel_kit \
            --add-module=/ngx_php7 > /dev/null && \
    make > /dev/null && make install > /dev/null

EXPOSE 8080

CMD /nginx/sbin/nginx -c /deploy/nginx_default.conf 
