FROM techempower/php7:0.1

ADD ./ /yii2
WORKDIR /yii2

RUN composer.phar install --no-progress

CMD service php7.2-fpm start && \
    nginx -c /yii2/deploy/nginx-fpm.conf -g "daemon off;"
