FROM dunglas/frankenphp
 
RUN install-php-extensions \
    intl \
	opcache \
	pcntl \
    pdo_mysql \
	zip > /dev/null
 
COPY --link . /app/

COPY --from=composer --link /usr/bin/composer /usr/local/bin/composer

RUN mkdir -p bootstrap/cache \
            storage/logs \
            storage/framework/sessions \
            storage/framework/views \
            storage/framework/cache

COPY --link deploy/conf/php.ini  /usr/local/etc/php

RUN composer require laravel/octane guzzlehttp/guzzle --update-no-dev --no-scripts --quiet
RUN php artisan optimize

RUN frankenphp -v

EXPOSE 8080

ENTRYPOINT ["php", "artisan", "octane:frankenphp", "--port=8080"]
