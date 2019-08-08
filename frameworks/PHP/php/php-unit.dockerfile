FROM nginx/unit:1.9.0-php7.0

RUN apt-get update -yqq > /dev/null && \
    apt-get install -yqq php7.0-mysql > /dev/null

ADD ./ /php
WORKDIR /php

# RUN if [ $(nproc) = 2 ]; then sed -i "s|pm.max_children = 1024|pm.max_children = 512|g" /etc/php/7.3/fpm/php-fpm.conf ; fi;

RUN unitd --control unix:/var/run/control.unit.sock && \
    curl -X PUT --data-binary @/php/deploy/nginx-unit.json --unix-socket \
        /var/run/control.unit.sock http://localhost/config 
