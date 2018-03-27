FROM tfb/php5:latest

ADD ./ /lithium
WORKDIR /lithium

RUN composer.phar install --no-progress

RUN chmod -R 777 /lithium

CMD service php5.6-fpm start && \
    nginx -c /lithium/deploy/nginx.conf -g "daemon off;"
