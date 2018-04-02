FROM techempower/php7-base:0.1

CMD service php7.2-fpm start && \
    nginx -c /php/deploy/nginx7.conf -g "daemon off;"
