FROM techempower/phalcon-base:0.1

CMD service php7.2-fpm start && \
    nginx -c /phalcon/deploy/nginx.conf -g "daemon off;"
