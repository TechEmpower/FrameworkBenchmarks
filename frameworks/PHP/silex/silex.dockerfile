FROM tfb/silex-base:latest

CMD service php7.2-fpm start && \
    nginx -c /silex/deploy/nginx.conf -g "daemon off;"
