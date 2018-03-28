FROM tfb/php7-base:latest

CMD service php7.2-fpm start && \
    nginx -c /php/deploy/nginx7.conf -g "daemon off;"
