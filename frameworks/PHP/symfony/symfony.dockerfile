FROM ubuntu:19.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php7.3 php7.3-common php7.3-cli php7.3-fpm php7.3-mysql  > /dev/null
RUN apt-get install php7.3-xml  > /dev/null

RUN apt-get install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/7.3/fpm/

ADD ./ /symfony
WORKDIR /symfony

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.3/fpm/php-fpm.conf ; fi;

ENV APP_ENV prod

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

RUN php bin/console cache:clear --env=prod --no-debug --no-warmup
RUN php bin/console cache:warmup --env=prod --no-debug

RUN mkdir -p /symfony/var/cache/dev
RUN chmod 777 -R /symfony/var/cache/dev

RUN mkdir -p /symfony/var/log
RUN chmod 777 -R /symfony/var/log

CMD service php7.3-fpm start && \
    nginx -c /symfony/deploy/nginx.conf -g "daemon off;"
