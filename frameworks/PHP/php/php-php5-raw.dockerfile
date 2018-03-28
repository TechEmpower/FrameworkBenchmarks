FROM tfb/php5-base:latest

CMD service php5.6-fpm start && \
    nginx -c /php/deploy/nginx5.conf -g "daemon off;"
