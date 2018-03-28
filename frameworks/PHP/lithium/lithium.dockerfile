FROM techempower/php5:0.1

ADD ./ /lithium
WORKDIR /lithium

RUN composer.phar install --no-progress

RUN chmod -R 777 /lithium

CMD service php5.6-fpm start && \
    nginx -c /lithium/deploy/nginx.conf -g "daemon off;"
