FROM techempower/php7:0.1

ADD ./ /codeigniter
WORKDIR /codeigniter

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /codeigniter/deploy/nginx-fpm.conf -g "daemon off;"
