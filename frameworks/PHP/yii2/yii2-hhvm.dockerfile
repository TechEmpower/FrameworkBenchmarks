FROM techempower/hhvm-php7:0.1

ADD ./ /yii2
WORKDIR /yii2

RUN composer.phar install --no-progress

CMD hhvm -m daemon --config /yii2/deploy/config.hdf && \
    nginx -c /yii2/deploy/nginx-hhvm.conf -g "daemon off;"
