FROM nginx/unit:1.22.0-php8.0

RUN docker-php-ext-install opcache pdo_mysql > /dev/null

ADD ./ /php
WORKDIR /php

COPY deploy/nginx-unit.json /docker-entrypoint.d/
# forward log to docker log collector
#RUN ln -sf /dev/stdout /var/log/unit.log

# RUN if [ $(nproc) = 2 ]; then sed -i "s|\"processes\": 128,|\"processes\": 64,|g" /php/deploy/nginx-unit.json ; fi;

EXPOSE 8080
