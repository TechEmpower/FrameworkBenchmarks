FROM techempower/php7:0.1

ADD ./ /zend
WORKDIR /zend

RUN mkdir -p data/cache
RUN chmod 777 data/cache

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /zend/deploy/nginx.conf -g "daemon off;"
