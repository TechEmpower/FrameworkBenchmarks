FROM unit:php8.4

RUN docker-php-ext-install pdo_mysql opcache > /dev/null

WORKDIR /php
COPY --link . .

RUN if [ $(nproc) = 2 ]; then sed -i "s|\"spare\": 168,|\"spare\": 64,|g" /php/deploy/nginx-unit.json ; fi;

#RUN more /php/deploy/nginx-unit.json

RUN unitd && \
    curl -X PUT --data-binary @/php/deploy/nginx-unit.json --unix-socket \
        /var/run/control.unit.sock http://localhost/config

ENTRYPOINT [ ]

EXPOSE 8080

CMD ["unitd", "--no-daemon", "--control", "unix:/var/run/control.unit.sock"]
