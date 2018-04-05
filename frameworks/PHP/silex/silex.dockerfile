FROM techempower/silex-base:0.1

CMD service php7.2-fpm start && \
    nginx -c /silex/deploy/nginx.conf -g "daemon off;"
