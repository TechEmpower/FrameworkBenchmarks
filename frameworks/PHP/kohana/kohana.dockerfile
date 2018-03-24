FROM tfb/php7:latest

ADD ./ /kohana
WORKDIR /kohana

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /kohana/deploy/nginx.conf -g "daemon off;"
