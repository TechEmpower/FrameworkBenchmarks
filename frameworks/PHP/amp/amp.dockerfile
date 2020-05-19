FROM ubuntu:19.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip php7.3 php7.3-common php7.3-cli php7.3-dev php7.3-mbstring composer curl build-essential > /dev/null

# An extension is required!
# We deal with concurrencies over 1k, which stream_select doesn't support.

RUN apt-get install -y php-pear php-dev libuv1-dev libuv1 > /dev/null
RUN pecl install uv-0.2.4 > /dev/null

ADD ./ /amp
WORKDIR /amp

COPY deploy/conf/* /etc/php/7.3/cli/conf.d/

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

CMD php /amp/vendor/bin/cluster /amp/server.php
