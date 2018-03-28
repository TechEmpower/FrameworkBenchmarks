FROM tfb/kumbiaphp-base:latest

CMD service php7.2-fpm start && \
    nginx -c /kumbiaphp/deploy/nginx.conf -g "daemon off;"
