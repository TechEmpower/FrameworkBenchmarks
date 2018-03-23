FROM tfb/php7:latest

ADD ./ /zend1
WORKDIR /zend1

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /zend1/deploy/nginx.conf -g "daemon off;"
