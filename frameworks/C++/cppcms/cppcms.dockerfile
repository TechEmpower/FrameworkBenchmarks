FROM techempower/cppcms-base:0.1

CMD nginx -c /cppcms/nginx.conf && ./mycppcms -c config-nginx-mysql.json
