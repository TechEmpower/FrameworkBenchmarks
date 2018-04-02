FROM techempower/php5-base:0.1

CMD service php5.6-fpm start && \
    nginx -c /php/deploy/nginx5.conf -g "daemon off;"
