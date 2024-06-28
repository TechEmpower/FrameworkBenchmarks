FROM dunglas/frankenphp

# add additional extensions here:
RUN install-php-extensions \
    intl \
    opcache \
    pdo_pgsql \
    zip > /dev/null

COPY --from=composer/composer:latest-bin --link /composer /usr/local/bin/composer

COPY --link deploy/Caddyfile /etc/caddy/Caddyfile
COPY --link deploy/conf/php.ini /usr/local/etc/php/

RUN echo "opcache.preload=/symfony/var/cache/prod/App_KernelProdContainer.preload.php" >> /usr/local/etc/php/php.ini && \
    echo "opcache.preload_user=root" >> /usr/local/etc/php/php.ini

WORKDIR /symfony
COPY --link . .

ENV FRANKENPHP_CONFIG="worker /symfony/public/runtime.php"
ENV APP_RUNTIME="Runtime\FrankenPhpSymfony\Runtime"
#ENV CADDY_DEBUG=debug
RUN composer install --optimize-autoloader --classmap-authoritative --no-dev --no-scripts --quiet
RUN cp deploy/postgresql/.env . && composer dump-env prod && bin/console cache:clear

EXPOSE 8080

RUN frankenphp -v
