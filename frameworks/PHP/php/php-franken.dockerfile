FROM dunglas/frankenphp:1.4-php8.4

WORKDIR /app

RUN install-php-extensions pdo_pgsql

COPY --link deploy/franken/ /php

EXPOSE 8080

ENTRYPOINT ["frankenphp", "run", "-c", "/php/Caddyfile"]