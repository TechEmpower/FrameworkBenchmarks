FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php > /dev/null && \
    apt-get update -yqq > /dev/null && apt-get upgrade -yqq > /dev/null

RUN apt-get install -yqq nginx git unzip \
    php8.3-fpm php8.3-mysql > /dev/null

COPY --link deploy/conf/* /etc/php/8.3/fpm/
WORKDIR /kumbiaphp
COPY --link . .

RUN git clone -b v1.2.1 --single-branch --depth 1 -q https://github.com/KumbiaPHP/KumbiaPHP.git vendor/Kumbia

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/8.3/fpm/php-fpm.conf ; fi;

EXPOSE 8080

CMD service php8.3-fpm start && \
    nginx -c /kumbiaphp/deploy/nginx.conf
