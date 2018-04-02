FROM techempower/fat-free-base:0.1

CMD service php7.2-fpm start && \
    nginx -c /fat-free/deploy/nginx.conf -g "daemon off;"
