FROM techempower/hhvm-php7:0.1

ADD ./ /hhvm_app
WORKDIR /hhvm_app

CMD hhvm -m daemon --config /hhvm_app/deploy/config.hdf && \
    nginx -c /hhvm_app/deploy/nginx.conf -g "daemon off;"
