FROM ubuntu:18.10

ENV PHP_VERSION="7.3"
ENV RR_VERSION="1.2.6"

ENV RR_URL="https://github.com/spiral/roadrunner/archive/v${RR_VERSION}.tar.gz"
ENV GOPATH="/php/go"

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq software-properties-common > /dev/null

RUN add-apt-repository -y ppa:ondrej/php && \
    apt-get update -yqq > /dev/null && \
    apt-get install -yqq \
        curl \
        git \
        php${PHP_VERSION}-cli \
        php${PHP_VERSION}-mysqli \
        php${PHP_VERSION}-apcu \
        php${PHP_VERSION}-opcache \
        golang-go > /dev/null

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

ADD ./ /php
WORKDIR /php
RUN chmod -R 777 /php

RUN mkdir "${GOPATH}"

RUN curl -sSL "${RR_URL}" | tar xzf - && \
    cd "roadrunner-${RR_VERSION}" && \
    go get -d ./... && \
    make && \
    cp rr /usr/local/bin/rr && \
    chmod +x /usr/local/bin/rr

RUN sed -i 's|WORKERS|'"$(nproc)"'|g' /php/deploy/.rr.yaml

RUN composer update --quiet --no-dev --optimize-autoloader --classmap-authoritative

CMD rr -c /php/deploy/.rr.yaml serve
