FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq
RUN apt install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-mysql > /dev/null
RUN apt install -yqq php7.2-xml > /dev/null

RUN apt install -yqq composer > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /symfony
WORKDIR /symfony

ENV APP_ENV=prod

RUN composer install --quiet

RUN php bin/console cache:clear --env=prod --no-debug --no-warmup
RUN php bin/console cache:warmup --env=prod --no-debug

RUN mkdir -p /symfony/var/cache/dev
RUN chmod 777 -R /symfony/var/cache/dev

RUN mkdir -p /symfony/var/log
RUN chmod 777 -R /symfony/var/log

CMD service php7.2-fpm start && \
    nginx -c /symfony/deploy/nginx.conf -g "daemon off;"
