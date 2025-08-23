FROM dunglas/frankenphp

# add additional extensions here:
RUN install-php-extensions \
    pdo_mysql \
    zip \
    opcache > /dev/null


COPY --link deploy/franken/Caddyfile /etc/frankenphp/Caddyfile
COPY --link deploy/conf/php.ini /usr/local/etc/php/

COPY --link . /php

# Worker mode 
#ENV FRANKENPHP_CONFIG="worker ./public/index.php"

#ENV CADDY_DEBUG=true

EXPOSE 8080
