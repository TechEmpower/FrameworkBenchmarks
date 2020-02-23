FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq wget git unzip libxml2-dev cmake make systemtap-sdt-dev \
                    zlibc zlib1g zlib1g-dev libpcre3 libpcre3-dev libargon2-0-dev libsodium-dev \
                    php7.4 php7.4-common php7.4-dev libphp7.4-embed php7.4-mysql nginx > /dev/null

ADD ./ ./

ENV NGINX_VERSION=1.17.8

RUN git clone -b v0.0.22 --single-branch --depth 1 https://github.com/rryqszq4/ngx_php7.git > /dev/null

RUN wget -q http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    export PHP_LIB=/usr/lib && \ 
    ./configure --user=www --group=www \
            --prefix=/nginx \
            --with-ld-opt="-Wl,-rpath,$PHP_LIB" \
            --add-module=/ngx_php7/third_party/ngx_devel_kit \
            --add-module=/ngx_php7 > /dev/null && \
    make > /dev/null && make install > /dev/null

CMD export WORKERS=$(( 3 * $(nproc) )) && \
    sed -i "s/worker_processes  auto/worker_processes $WORKERS/g" /deploy/nginx.conf && \
    /nginx/sbin/nginx -c /deploy/nginx.conf 
