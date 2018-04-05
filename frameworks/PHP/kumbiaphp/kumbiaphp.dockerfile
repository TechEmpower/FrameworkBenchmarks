FROM techempower/kumbiaphp-base:0.1

CMD service php7.2-fpm start && \
    nginx -c /kumbiaphp/deploy/nginx.conf -g "daemon off;"
