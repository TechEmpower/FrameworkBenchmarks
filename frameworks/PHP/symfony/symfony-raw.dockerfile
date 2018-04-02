FROM techempower/symfony-base:0.1

CMD service php7.2-fpm start && \
    nginx -c /symfony/deploy/nginx.conf -g "daemon off;"
