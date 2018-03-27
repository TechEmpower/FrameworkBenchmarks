FROM tfb/hhvm-php7:latest

ADD ./ /yii2
WORKDIR /yii2

RUN composer.phar install --no-progress

CMD hhvm -m daemon --config /yii2/deploy/config.hdf && \
    nginx -c /yii2/deploy/nginx-hhvm.conf -g "daemon off;"
