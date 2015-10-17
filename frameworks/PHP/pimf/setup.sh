#!/bin/bash

fw_depends php nginx composer cphalcon

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' app/config.app.php
sed -i 's|".*/FrameworkBenchmarks/php-pimf|"'"${TROOT}"'|g' deploy/php-pimf
sed -i 's|Directory .*/FrameworkBenchmarks/php-pimf|Directory '"${TROOT}"'|g' deploy/php-pimf
sed -i 's|root .*/FrameworkBenchmarks/php-pimf|root '"${TROOT}"'|g' deploy/php-pimf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf
sed -i 's|root .*/FrameworkBenchmarks/php-pimf|root '"${TROOT}"'|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
