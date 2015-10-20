#!/bin/bash

fw_depends php composer nginx

sed -i 's|database_host: .*|database_host: '"${DBHOST}"'|g' app/config/parameters.yml
sed -i 's|root .*/FrameworkBenchmarks/php-symfony2-stripped| root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php app/console cache:clear --env=prod --no-debug --no-warmup
php app/console cache:warmup --env=prod --no-debug

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
