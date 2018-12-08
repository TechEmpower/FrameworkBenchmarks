FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq  > /dev/null
RUN apt-get install -yqq wget git unzip libxml2-dev cmake make

ADD ./ ./

ENV PHP_VERSION=7.2.12
ENV NGINX_VERSION=1.12.2

RUN wget http://php.net/distributions/php-${PHP_VERSION}.tar.gz && \
    tar xf php-${PHP_VERSION}.tar.gz && \
    cd php-${PHP_VERSION}  && \
    ./configure --prefix=/php7 --enable-embed && \
    make && make install && \
    cd ..

RUN git clone https://github.com/rryqszq4/ngx_php7.git

RUN wget http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxvf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    export PHP_CONFIG=/php7/bin/php-config && \
    export PHP_BIN=/php7/bin && \
    export PHP_INC=/php7/include && \
    export PHP_LIB=/php7/lib && \
    ./configure --user=www --group=www \
            --prefix=/nginx \
            --with-ld-opt="-Wl,-rpath,$PHP_LIB" \
            --add-module=/ngx_php7/third_party/ngx_devel_kit \
            --add-module=/ngx_php7 && \
    make && make install 


CMD /nginx/sbin/nginx -c /deploy/nginx_php.conf
