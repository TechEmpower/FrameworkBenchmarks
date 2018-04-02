FROM techempower/cygnite-base:0.1

CMD service php5.6-fpm start && \
    nginx -c /cygnite/deploy/nginx.conf -g "daemon off;"
