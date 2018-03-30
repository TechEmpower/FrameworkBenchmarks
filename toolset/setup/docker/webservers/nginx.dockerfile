FROM techempower/base:0.1

RUN apt install -yqq libpcre3 libpcre3-dev zlib1g-dev

ENV NGINX_HOME="/nginx"
ENV NGINX_VERSION="1.12.2"

RUN wget http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar xf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    echo "Configuring nginx..." && \
    ./configure --prefix=/nginx && \
    echo "Compiling and installing nginx..." && \
    make --quiet && \
    make --quiet install

ENV PATH=/nginx/sbin:${PATH}
