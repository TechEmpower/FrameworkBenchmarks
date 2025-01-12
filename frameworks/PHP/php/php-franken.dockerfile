FROM dunglas/frankenphp

WORKDIR /app

RUN install-php-extensions pdo_pgsql

COPY --link deploy/franken/ /php

EXPOSE 8080

ENTRYPOINT ["frankenphp", "run", "-c", "/php/Caddyfile"]