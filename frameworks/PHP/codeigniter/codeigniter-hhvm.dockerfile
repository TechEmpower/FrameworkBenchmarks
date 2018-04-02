FROM techempower/hhvm-php7:0.1

ADD ./ /codeigniter
WORKDIR /codeigniter

RUN composer.phar install --no-progress

CMD hhvm -m daemon --config /codeigniter/deploy/config.hdf && \
    nginx -c /codeigniter/deploy/nginx-hhvm.conf -g "daemon off;"
