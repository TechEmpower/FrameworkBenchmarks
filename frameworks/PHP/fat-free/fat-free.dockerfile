FROM tfb/fat-free-base:latest

CMD service php7.2-fpm start && \
    nginx -c /fat-free/deploy/nginx.conf -g "daemon off;"
