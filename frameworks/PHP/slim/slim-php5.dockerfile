FROM tfb/php5:latest

ADD ./ /slim
WORKDIR /slim

RUN composer.phar install --no-progress

RUN chmod -R 777 /slim

CMD service php5.6-fpm start && \
    nginx -c /slim/deploy/nginx-fpm-5.conf -g "daemon off;"
