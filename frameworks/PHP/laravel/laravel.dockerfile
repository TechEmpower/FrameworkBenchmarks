FROM techempower/php7:0.1

ADD ./ /laravel
WORKDIR /laravel

RUN composer.phar install --no-progress

RUN php artisan config:cache
RUN php artisan route:cache

RUN chmod -R 777 /laravel

CMD service php7.2-fpm start && \
    nginx -c /laravel/deploy/nginx.conf -g "daemon off;"
