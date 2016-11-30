#!/bin/bash

fw_depends php5 nginx composer

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' apps/configs/database.php
sed -i 's|root .*/FrameworkBenchmarks/cygnite|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c ${TROOT}/deploy/nginx.conf
