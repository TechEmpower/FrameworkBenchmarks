FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq nginx git unzip php7.4 php7.4-common php7.4-cli php7.4-fpm php7.4-pgsql  > /dev/null

COPY deploy/conf/* /etc/php/7.4/fpm/

ADD ./ /php
WORKDIR /php

RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" dbraw.php
RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" dbquery.php
RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" fortune.php
RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" updateraw.php

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.4/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /php

CMD service php7.4-fpm start && \
    nginx -c /php/deploy/nginx7.conf -g "daemon off;"
