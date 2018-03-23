FROM tfb/php7:latest

ADD ./ /symfony
WORKDIR /symfony

ENV APP_ENV=prod

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /symfony/deploy/nginx.conf -g "daemon off;"
