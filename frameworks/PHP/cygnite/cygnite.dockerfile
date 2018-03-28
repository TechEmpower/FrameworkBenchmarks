FROM tfb/cygnite-base:latest

CMD service php5.6-fpm start && \
    nginx -c /cygnite/deploy/nginx.conf -g "daemon off;"
