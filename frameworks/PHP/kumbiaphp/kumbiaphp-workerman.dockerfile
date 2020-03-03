FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq git unzip php7.4 php7.4-common php7.4-cli php7.4-mysql  > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN apt-get install -y php-pear php-dev libevent-dev > /dev/null
RUN printf "\n\n /usr/lib/x86_64-linux-gnu/\n\n\nno\n\n\n" | pecl install event > /dev/null && echo "extension=event.so" > /etc/php/7.4/cli/conf.d/event.ini

COPY deploy/conf/cliphp.ini /etc/php/7.4/cli/php.ini

ADD ./ /kumbiaphp
WORKDIR /kumbiaphp

RUN git clone -b dev --single-branch --depth 1 https://github.com/KumbiaPHP/KumbiaPHP.git vendor/Kumbia
RUN git clone -b master --single-branch --depth 1 https://github.com/KumbiaPHP/ActiveRecord.git vendor/Kumbia/ActiveRecord

RUN sed -i "s|header(|\\\Workerman\\\Protocols\\\Http::header(|g" bench/app/controllers/*.php
#RUN sed -i "s|mysql:host|pgsql:host|g" bench/app/config/databases.php

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

CMD php server.php start
