FROM techempower/php7:0.1

ADD ./ /kohana
WORKDIR /kohana

RUN composer.phar install --no-progress

RUN chmod -R 777 /kohana

CMD service php7.2-fpm start && \
    nginx -c /kohana/deploy/nginx.conf -g "daemon off;"
