FROM tfb/cppcms-base:latest

CMD nginx -c /cppcms/nginx.conf && ./mycppcms -c config-nginx-postgresql.json
