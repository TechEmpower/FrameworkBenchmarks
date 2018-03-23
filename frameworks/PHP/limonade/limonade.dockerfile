FROM tfb/php7:latest

ADD ./ /limonade
WORKDIR /limonade

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /limonade/deploy/nginx.conf -g "daemon off;"
