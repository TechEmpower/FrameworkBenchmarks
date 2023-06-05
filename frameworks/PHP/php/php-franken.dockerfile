FROM dunglas/frankenphp

# add additional extensions here:
RUN install-php-extensions \
    pdo_mysql \
    zip \
    opcache


COPY deploy/franken/Caddyfile /etc/Caddyfile

ADD . /php

# Worker mode 
#ENV FRANKENPHP_CONFIG="worker ./public/index.php"

#ENV CADDY_DEBUG=true

EXPOSE 8080
