FROM ubuntu:19.10

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php7.4 php7.4-common php7.4-cli php7.4-pgsql php7.4-xml > /dev/null

RUN apt-get install -yqq composer > /dev/null

RUN apt-get install -y php-pear php-dev libevent-dev > /dev/null
RUN printf "\n\n /usr/lib/x86_64-linux-gnu/\n\n\nno\n\n\n" | pecl install event > /dev/null && echo "extension=event.so" > /etc/php/7.4/cli/conf.d/event.ini

COPY php.ini /etc/php/7.4/cli/php.ini

ADD ./ /workerman
WORKDIR /workerman

RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" app.php

RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --quiet

CMD php /workerman/server.php start
