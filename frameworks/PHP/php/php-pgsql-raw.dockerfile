FROM ubuntu:16.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -yqq && apt-get install -yqq software-properties-common > /dev/null
RUN LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
RUN apt update -yqq
RUN apt install -yqq nginx git unzip php7.2 php7.2-common php7.2-cli php7.2-fpm php7.2-pgsql  > /dev/null

COPY deploy/conf/* /etc/php/7.2/fpm/

ADD ./ /php
WORKDIR /php

RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" dbraw.php
RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" fortune.php
RUN sed -i "s|PDO('mysql:|PDO('pgsql:|g" updateraw.php

RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 2048|pm.max_children = 512|g" /etc/php/7.2/fpm/php-fpm.conf ; fi;

RUN chmod -R 777 /php

CMD service php7.2-fpm start && \
    nginx -c /php/deploy/nginx7.conf -g "daemon off;"
