FROM tfb/hhvm-php7:latest

ADD ./ /hhvm_app
WORKDIR /hhvm_app

CMD hhvm -m daemon --config /hhvm_app/deploy/config.hdf && \
    nginx -c /hhvm_app/deploy/nginx.conf -g "daemon off;"
