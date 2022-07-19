FROM nginx/unit:1.27.0-php8.1

ADD . /php
WORKDIR /php

RUN docker-php-ext-install pdo_mysql opcache > /dev/null
RUN if [ $(nproc) = 2 ]; then sed -i "s|\"processes\": 84,|\"processes\": 64,|g" /php/deploy/nginx-unit.json ; fi;

COPY deploy/nginx-unit.json /docker-entrypoint.d/nginx-unit.json

EXPOSE 8080

CMD ["unitd", "--no-daemon", "--control", "unix:/var/run/control.unit.sock"]
