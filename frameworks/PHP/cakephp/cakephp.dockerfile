FROM tfb/php5:latest

ADD ./ /cakephp
WORKDIR /cakephp

RUN mkdir -p app/tmp/cache/models
RUN mkdir -p app/tmp/cache/persistent
RUN mkdir -p app/tmp/logs
RUN chmod -R 777 app/tmp

RUN composer.phar install --no-progress

CMD service php5.6-fpm start && \
    nginx -c /cakephp/deploy/nginx.conf -g "daemon off;"
