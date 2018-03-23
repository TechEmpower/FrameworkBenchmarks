FROM tfb/php5:latest

ADD ./ /cakephp
WORKDIR /cakephp

RUN composer.phar install --no-progress

CMD service php5.6-fpm start && \
    nginx -c /cakephp/deploy/nginx.conf -g "daemon off;"
