FROM ubuntu:20.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip \
    php8.0-fpm php8.0-mysql  > /dev/null

COPY deploy/conf/* /etc/php/8.0/fpm/

ADD ./ /kumbiaphp
WORKDIR /kumbiaphp

RUN git clone -b v1.1.4 --single-branch --depth 1 https://github.com/KumbiaPHP/KumbiaPHP.git vendor/Kumbia
RUN git clone -b dev --single-branch --depth 1 https://github.com/KumbiaPHP/ActiveRecord.git vendor/Kumbia/ActiveRecord

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.0/fpm/php-fpm.conf ; fi;

EXPOSE 8080

CMD service php8.0-fpm start && \
    nginx -c /kumbiaphp/deploy/nginx.conf
